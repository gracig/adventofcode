module IntCode (
    compute,
    computeWithLog,
    fromList,
    exec,
    execWithInput,
    input,
    set,
    get,
    parInput,
    execChain,
    State(sts,acc,pgm,ptr,reg),
    Status(Done,Input,Output,Failed,Begin),
) where

import qualified Data.Map as Map
import qualified Control.Monad.Writer as Writer
import Control.Monad
import Control.Applicative

data Instruction = Instruction Integer Integer Integer Integer Integer
data PmMode = Position | Immediate | Relative deriving (Eq, Show)
data Opcode = Sum PmMode PmMode PmMode
            | Mul PmMode PmMode PmMode
            | Inp PmMode
            | Out PmMode
            | Jpt PmMode PmMode
            | Jpf PmMode PmMode
            | Lt  PmMode PmMode PmMode
            | Eq  PmMode PmMode PmMode
            | Rel PmMode
            | End
data Status = Begin
            | Done 
            | Failed 
            | Output 
            | Input deriving (Eq, Show)

data State  = State { lbl::String, sts::Status, ptr::Integer, pgm::Map.Map Integer Integer, reg::Integer, acc::[Integer]} deriving (Show)

compute = fst . Writer.runWriter
computeWithLog = Writer.runWriter

            
fromList :: [(Integer,Integer)] -> State
fromList code = State {lbl="code",sts=Begin, ptr=0, pgm=Map.fromList code, reg=0, acc=[]}

set :: Integer -> Integer -> State -> Writer.Writer [String] State
set k v s = do
    Writer.tell["Set value "++ show v ++ " at index " ++ show k]
    return s{ pgm = Map.insert k v (pgm s)}

get :: Integer -> State -> Writer.Writer [String] (Maybe Integer)
get k s = do
    Writer.tell["Get value at index " ++ show k]
    return $  lookup' (pgm s) k

head' :: [Integer] -> Integer
head' xs = case xs of
    [] -> 0
    _  -> head xs

parInput' :: [ Integer ] -> [ State ] -> Writer.Writer [String] [State] 
parInput' vs ss = do
    let zipped = zipWith3 (
                \i v s -> 
                    computeWithLog $ return s {lbl=(lbl s)++ show i} >>= execWithInput (\x -> v) False >>= exec 
            ) [1..] vs ss
    forM_ zipped  (\(_,log) -> Writer.tell log)
    return $ map (\(s,log) -> s) zipped 

execChain' :: Integer -> [State] -> Writer.Writer [String] (Maybe Integer)
execChain' v ss = do
    Writer.tell ["Executing chained intcode machines"]
    let (v',rchain) = foldl  ( \(v',ss') s -> 
                            let ss''@(s',log) = computeWithLog $ input v' s  >>= exec
                            in  (head (acc s'), ss'':ss')
                        ) (v,[]) ss
        chain = reverse rchain
        chain' = reverse $ map fst chain
    forM_ chain  (\(_,log) -> Writer.tell log)
    case (sts . fst . head ) rchain of 
        Input -> do
            Writer.tell ["Starting another cycle with "++ show v']
            --execChain' v' chain'
            return Nothing
        Done -> return (Just v')
        _    -> return Nothing
    

parInput :: [ Integer ] -> [ State ] -> Writer.Writer [String] [Writer.Writer [String] State] 
parInput vs ss = 
    return $ zipWith3 (\i v s -> return s {lbl=(lbl s)++ show i} >>= execWithInput (\x -> v) False >>= exec ) [1..] vs ss 

execChain :: Integer -> [Writer.Writer [String] State] -> Writer.Writer [String] State
execChain v ss = do
    Writer.tell ["Executing chained intcode machines"]
    let (v',rchain)  =  foldl ( \(v',ss') s ->
                                    let s'  = s >>= input v' >>= exec
                                        v'' = compute $  s' >>= (\x -> return $ head (acc x))  
                                    in (v'',s':ss')
                        ) (v,[]) ss
    s <- head rchain
    case sts s of 
        Input -> do
            Writer.tell ["Starting another cycle with "++ show v']
            execChain v' (reverse rchain)
        Done -> return s        
execWithInput :: ( State -> Integer ) -> Bool -> State -> Writer.Writer [String] State
execWithInput f c s= do 
    s' <- exec s
    case sts s' of
        Input  -> do
            s'' <- input ( f s' ) s'
            if c then do 
                    Writer.tell [(lbl s'') ++ ": flag continue is on"]
                    exec s''
                 else return s''
        Done   -> return s'
        Failed -> return s'
        _      -> fatal s' $ "Expecting states Input, Done or Failed. found: " ++ show (sts s')
    
exec :: State -> Writer.Writer [String] State
exec s = case lookup' (pgm s) (ptr s) >>= instructionFromValue >>= opcodeFromInstruction of
        Nothing -> fatal s $ "EXEC - could not parse instruction at " ++ show (ptr s)
        Just op -> do 
            Writer.tell[ (lbl s)++": Exec instruction at index " ++ show (ptr s)]
            case op of
                Sum m1 m2 m3 -> opsum m1 m2 m3 s >>= exec
                Mul m1 m2 m3 -> opmul m1 m2 m3 s >>= exec
                Lt  m1 m2 m3 -> oplt  m1 m2 m3 s >>= exec
                Eq  m1 m2 m3 -> opeq  m1 m2 m3 s >>= exec
                Jpt m1 m2    -> opjpt m1 m2 s >>= exec
                Jpf m1 m2    -> opjpf m1 m2 s >>= exec
                Rel m1       -> oprel m1 s >>= exec
                Out m1       -> opout m1 s >>= exec
                Inp m1       -> opinp m1 s 
                End          -> opend s

lookup':: Map.Map Integer Integer -> Integer -> Maybe Integer
lookup' m k
    | k < 0 = Nothing
    | otherwise = case Map.lookup k m  of
        Nothing -> Just 0
        Just x  -> Just x        

rbase :: State -> Integer
rbase s = case Map.lookup (-1) (pgm s) of
    Nothing -> 0
    Just x  -> x

value :: PmMode -> Integer -> State -> Maybe Integer
value m o s=
    let pg = pgm s
        pt = (ptr s) + o
        rb = rbase s
    in  case m of 
        Immediate -> lookup' pg pt
        Position  -> lookup' pg pt >>= lookup' pg
        Relative  -> lookup' pg pt >>= (\x -> Just (x + rb)) >>= lookup' pg

ref :: PmMode -> Integer -> State -> Maybe Integer
ref m o s=
    let pg = pgm s
        pt = (ptr s) + o
        rb = rbase s
    in  case m of 
        Immediate -> Just pt
        Position  -> lookup' pg pt
        Relative  -> lookup' pg pt >>= (\x -> Just (x + rb))

just' :: Maybe a -> a
just' x = case x of
    Nothing -> error "just should always be different than nothing"
    Just x -> x        

input :: Integer -> State -> Writer.Writer [String] State
input v s = do
    Writer.tell [ (lbl s)++": Store value "++ show v ++ " at index "++ show (reg s) ]
    return s{pgm=Map.insert (reg s) v (pgm s)}

move :: (Integer->Integer) -> State -> Writer.Writer [String] State
move f s = do
    let src = ptr s
        dst = f src 
    Writer.tell [ (lbl s)++": Move pointer from "++ show src ++ " to "++ show dst]
    return s{ptr = dst}

fatal :: State -> String -> Writer.Writer [String] State
fatal s msg = do
    Writer.tell [ (lbl s)++": END - Abnormal exit - " ++ msg ++ " - at "++ show (ptr s) ]
    return s { sts=Failed}
        
opend :: State -> Writer.Writer [String] State
opend s = do
    Writer.tell [ (lbl s)++": END - Normal exit"]
    return s { sts=Done}

opsum :: PmMode -> PmMode -> PmMode -> State -> Writer.Writer [String] State
opsum m1 m2 m3 s = 
    let p1 = value m1 1 s
        p2 = value m2 2 s
        p3 = ref   m3 3 s
        vl = ( (+) <$> p1 <*> p2 )
    in  if any (==Nothing) [p1,p2,p3]
        then fatal s $ "SUM - invalid parameters [p1,p2,p3] = " ++ show [p1,p2,p3]
        else do
            Writer.tell [ (lbl s)++": SUM "++ show p1 ++ " + " ++ show p2 ++ " = " ++ show vl ]
            input (just' vl) s{ reg = just' p3}  >>= move (+4)

opmul :: PmMode -> PmMode -> PmMode -> State -> Writer.Writer [String] State
opmul m1 m2 m3 s = 
    let p1 = value m1 1 s
        p2 = value m2 2 s
        p3 = ref   m3 3 s
        vl = ( (*) <$> p1 <*> p2 )
    in  if any (==Nothing) [p1,p2,p3]
        then fatal s $ "MUL - invalid parameters [p1,p2,p3] = " ++ show [p1,p2,p3]
        else do
            Writer.tell [ (lbl s)++": MUL "++ show p1 ++ " * " ++ show p2 ++ " = " ++ show vl ]
            input  (just' vl)  s{ reg = just' p3} >>= move (+4)

oplt :: PmMode -> PmMode -> PmMode -> State -> Writer.Writer [String] State
oplt m1 m2 m3 s = 
    let p1 = value m1 1 s
        p2 = value m2 2 s
        p3 = ref   m3 3 s
        vl = case (<) <$> p1 <*> p2 of
            Nothing -> Nothing
            Just True -> Just 1
            Just False -> Just 0
    in  if any (==Nothing) [p1,p2,p3]
        then fatal s $ "LT - invalid parameters [p1,p2,p3] = " ++ show [p1,p2,p3]
        else do
            Writer.tell [ (lbl s)++": LT "++ show p1 ++ " < " ++ show p2 ++ " == " ++ show vl]
            input  (just' vl)  s{ reg = just' p3} >>= move (+4)

opeq :: PmMode -> PmMode -> PmMode -> State -> Writer.Writer [String] State
opeq m1 m2 m3 s = 
    let p1 = value m1 1 s
        p2 = value m2 2 s
        p3 = ref   m3 3 s
        vl = case (==) <$> p1 <*> p2 of
            Nothing -> Nothing
            Just True -> Just 1
            Just False -> Just 0
    in  if any (==Nothing) [p1,p2,p3]
        then fatal s $ "EQ - invalid parameters [p1,p2,p3] = " ++ show [p1,p2,p3]
        else do
            Writer.tell [ (lbl s)++ ": EQ "++ show p1 ++ " < " ++ show p2 ++ " == " ++ show vl]
            input  (just' vl) s{ reg = just' p3} >>= move (+4)

opjpt :: PmMode -> PmMode -> State -> Writer.Writer [String] State
opjpt m1 m2 s = 
    let p1 = value m1 1 s
        p2 = value m2 2 s
        condition = (/=0) <$> p1
        whenTrue  = \_ -> just' p2
        whenFalse = (+3)
    in  if any (==Nothing) [p1,p2]
        then fatal s $ "JPT - invalid parameters [p1,p2] = " ++ show [p1,p2]
        else do
            Writer.tell [ (lbl s)++": JPT "++ (show p1) ]
            case condition of
                Nothing -> fatal s "JPT - fail processing jump value"
                Just True -> move whenTrue s
                Just False -> move whenFalse s

opjpf :: PmMode -> PmMode -> State -> Writer.Writer [String] State
opjpf m1 m2 s = 
    let p1 = value m1 1 s
        p2 = value m2 2 s
        condition = (==0) <$> p1
        whenTrue  = \_ -> just' p2
        whenFalse = (+3) 
    in  if any (==Nothing) [p1,p2]
        then fatal s $ "JPF - invalid parameters [p1,p2] = " ++ show [p1,p2]
        else do
            Writer.tell [ (lbl s)++": JPF "++ show p1 ]
            case condition of
                Nothing -> fatal s "JPF - fail processing jump value"
                Just True -> move whenTrue s
                Just False -> move whenFalse s

oprel :: PmMode -> State -> Writer.Writer [String] State
oprel m1 s =
    let p1  = value m1 1 s
        src = rbase s
        dst = (+) <$> Just src <*> p1
    in  if any (==Nothing) [p1]
        then fatal s $ "REL - invalid parameters [p1] = " ++ show [p1]
        else do
            Writer.tell [ (lbl s)++": relative base from " ++ show src ++ " to " ++ show dst]
            return s { pgm = Map.insert (-1) (just' dst) (pgm s)} >>= move(+2)
     
opinp :: PmMode -> State -> Writer.Writer [String] State
opinp m1 s = 
    let p1 = ref m1 1 s
    in  if any (==Nothing) [p1] 
        then fatal s $ "INPUT - invalid parameters [p1] = " ++ show [p1]
        else do
            Writer.tell [ (lbl s)++": INPUT provide an input value to be inserted at " ++ show p1 ]
            return s{ reg = just' p1 , sts = Input } >>= move (+2)

opout :: PmMode -> State -> Writer.Writer [String] State
opout m1 s = 
    let p1 = value m1 1 s
    in  if any (==Nothing) [p1]
        then fatal s $ "OUTPUT - invalid parameters [p1] = " ++ show [p1]
        else do
            Writer.tell [ (lbl s)++": OUTPUT - " ++ show p1 ]
            return s{ reg = just' p1 , sts = Output, acc=(just' p1):(acc s) } >>= move (+2)
            
instructionFromValue :: Integer -> Maybe Instruction
instructionFromValue v = case digits v of
    []          -> Nothing 
    [e]         -> Just ( Instruction 0 0 0 0 e )
    [d,e]       -> Just ( Instruction 0 0 0 d e )
    [c,d,e]     -> Just ( Instruction 0 0 c d e )
    [b,c,d,e]   -> Just ( Instruction 0 b c d e )
    [a,b,c,d,e] -> Just ( Instruction a b c d e )
    xs          -> Nothing
    where
        digits :: Integral x => x -> [x]
        digits x = case x of
            0       ->  []
            x       ->  digits (x `div` 10) ++ [ x `mod` 10 ]

opcodeFromInstruction :: Instruction -> Maybe Opcode
opcodeFromInstruction instruction = case instruction of
    Instruction a b c d e 
        | (pmode a) == Nothing  -> Nothing
        | (pmode b) == Nothing  -> Nothing
        | (pmode c) == Nothing  -> Nothing
        | opcode    == 1        -> Just $ Sum pc pb pa
        | opcode    == 2        -> Just $ Mul pc pb pa
        | opcode    == 3        -> Just $ Inp pc
        | opcode    == 4        -> Just $ Out pc
        | opcode    == 5        -> Just $ Jpt pc pb
        | opcode    == 6        -> Just $ Jpf pc pb
        | opcode    == 7        -> Just $ Lt  pc pb pa
        | opcode    == 8        -> Just $ Eq  pc pb pa
        | opcode    == 9        -> Just $ Rel pc
        | opcode    == 99       -> Just $ End 
        | otherwise             -> Nothing
        where
            opcode = d*10 + e
            pa = rpmode a 
            pb = rpmode b 
            pc = rpmode c
            pmode x = case x of
                0 -> Just Position
                1 -> Just Immediate
                2 -> Just Relative
                x -> Nothing
            rpmode x = case pmode x of
                Nothing  -> error "position from nothing" 
                Just x   -> x
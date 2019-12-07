import qualified Data.Map as Map
import Control.Applicative
import System.IO
import System.Environment   

main = do
    [f] <- getArgs
    input <- readFile f
    let program = Map.fromList $ zip [0..]  [ read x::Integer | x <- split ',' $ input ]
    compute program

compute :: Memory -> IO ()
compute = computeAt 0

data PmMode = Position | Immediate deriving (Eq, Show)
data Opcode = Sum PmMode PmMode PmMode
            | Mul PmMode PmMode PmMode
            | Inp PmMode
            | Ret PmMode
            | Jpt PmMode PmMode
            | Jpf PmMode PmMode
            | Lt  PmMode PmMode PmMode
            | Eq  PmMode PmMode PmMode
            | End
data Instruction = Instruction Integer Integer Integer Integer Integer
type Index  = Integer
type Value  = Integer
type Memory = Map.Map Index Value

computeAt :: Index -> Memory -> IO()
computeAt idx mem = case Map.lookup idx mem >>= instructionFromValue >>= opcodeFromInstruction of
        Nothing -> return ()
        Just op -> case op of
            Sum m1 m2 m3 
                | p1 == Nothing ->  return ()
                | p2 == Nothing ->  return ()
                | p3 == Nothing ->  return ()
                | vl == Nothing ->  return ()
                | otherwise     ->  do
--                    putStrLn $ "[SUM]:" ++ (show vl) ++ " stored at address " ++ (show p3) 
                    computeAt ni ( setValue p3 vl mem )
                where
                    p1 = getValue m1 1
                    p2 = getValue m2 2
                    p3 = getRef   m3 3
                    vl = (+) <$> p1 <*> p2
                    ni = idx + 4
            Mul m1 m2 m3 
                | p1 == Nothing ->  return ()
                | p2 == Nothing ->  return ()
                | p3 == Nothing ->  return ()
                | vl == Nothing ->  return ()
                | otherwise     ->  do
--                    putStrLn $ "[MUL]:" ++ (show [m1,m2,m3] ) ++ (show [p1,p2,p3,vl] )++ " stored at address " ++ (show p3) 
                    computeAt ni ( setValue p3 vl mem )
                where
                    p1 = getValue m1 1
                    p2 = getValue m2 2
                    p3 = getRef   m3 3
                    vl = (*) <$> p1 <*> p2
                    ni = idx + 4
            Lt  m1 m2 m3 
                | p1 == Nothing ->  return ()
                | p2 == Nothing ->  return ()
                | p3 == Nothing ->  return ()
                | vl == Nothing ->  return ()
                | otherwise     ->  do
--                    putStrLn $ "[LT ]:" ++ (show [m1,m2,m3] ) ++ (show [p1,p2,p3,vl] )++ " stored at address " ++ (show p3) 
                    computeAt ni ( setValue p3 vl mem )
                where
                    p1 = getValue m1 1
                    p2 = getValue m2 2
                    p3 = getRef   m3 3
                    vl = case (<) <$> p1 <*> p2  of
                        Nothing     -> Nothing 
                        Just True   -> Just 1
                        Just False  -> Just 0
                    ni = idx + 4
            Eq  m1 m2 m3 
                | p1 == Nothing ->  return ()
                | p2 == Nothing ->  return ()
                | p3 == Nothing ->  return ()
                | vl == Nothing ->  return ()
                | otherwise     ->  do
--                    putStrLn $ "[EQ ]:" ++ (show [m1,m2,m3] ) ++ (show [p1,p2,p3,vl] )++ " stored at address " ++ (show p3) 
                    computeAt ni ( setValue p3 vl mem )
                where
                    p1 = getValue m1 1
                    p2 = getValue m2 2
                    p3 = getRef   m3 3
                    vl = case (==) <$> p1 <*> p2  of
                        Nothing     -> Nothing 
                        Just True   -> Just 1
                        Just False  -> Just 0
                    ni = idx + 4
            Jpt m1 m2 
                | p1 == Nothing ->  return ()
                | p2 == Nothing ->  return ()
                | otherwise     ->  do
--                    putStrLn $ "[JMT]:" ++ (show [m1,m2] ) ++ (show [p1,p2] ) 
                    computeAt ni mem 
                where
                    p1 = getValue m1 1
                    p2 = getValue m2 2
                    ni = case ((/=0) <$> p1, p2) of
                        (Just True, Just v2) -> v2
                        (_,_) -> idx + 3
            Jpf m1 m2 
                | p1 == Nothing ->  return ()
                | p2 == Nothing ->  return ()
                | otherwise     ->  do
--                    putStrLn $ "[JMF]:" ++ (show [m1,m2] ) ++ (show [p1,p2] ) 
                    computeAt ni mem 
                where
                    p1 = getValue m1 1
                    p2 = getValue m2 2
                    ni = case ((==0) <$> p1, p2) of
                        (Just True, Just v2) -> v2
                        (_,_) -> idx + 3 
            Inp m1
                | p1 == Nothing ->  return ()
                | otherwise     ->  do
                    hPutStr stderr "[IN ]: "
                    hFlush stderr
                    vl <- getInteger               
                    computeAt ni ( setValue p1 (Just vl) mem )
                where
                    p1 = getRef m1 1
                    ni = idx + 2
            Ret m1
                | p1 == Nothing ->  return ()
                | otherwise     ->  do
                    hPutStr stderr $ "[OUT]: "
                    hFlush  stderr
                    putStrLn    ( case p1 of 
                                    Nothing-> "Nothing"
                                    Just x -> show x 
                                )
                    computeAt ni mem
                where
                    p1 = getValue m1 1
                    ni = idx + 2
            End ->  hPutStrLn stderr "status code 0"
            where
                get::Map.Map Integer Integer -> Maybe Integer -> Maybe Integer
                get m k = case (m,k) of
                    (m,Just i)  ->  Map.lookup i m
                    (m,Nothing) ->  Nothing
                g = get mem
                getValue :: PmMode -> Integer -> Maybe Integer
                getValue mode offset = case mode of 
                    Position  -> g $ g $ fmap (+offset) (Just idx)
                    Immediate -> g $ fmap (+offset) (Just idx)    
                getRef :: PmMode -> Integer -> Maybe Integer
                getRef mode offset = case mode of 
                    Position  -> g $ fmap (+offset) (Just idx)
                    Immediate -> fmap (+offset) (Just idx)     
                setValue :: Maybe Index -> Maybe Value -> Memory -> Memory
                setValue idx val mem = case (idx,val) of
                    (Nothing,_)         -> error "idx is Nothing"
                    (_,Nothing)         -> error "val is Nothing"
                    (Just k, Just v)    -> Map.insert k v mem 
                getInteger :: IO Integer
                getInteger =  readLn

instructionFromValue :: Value -> Maybe Instruction
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
        | opcode    == 4        -> Just $ Ret pc
        | opcode    == 5        -> Just $ Jpt pc pb
        | opcode    == 6        -> Just $ Jpf pc pb
        | opcode    == 7        -> Just $ Lt  pc pb pa
        | opcode    == 8        -> Just $ Eq  pc pb pa
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
                x -> Nothing
            rpmode x = case pmode x of
                Nothing  -> error "position from nothing" 
                Just x   -> x 

split :: Char -> String -> [String]
split c s =  
    case dropWhile (==c) s of
        "" -> []
        s' -> w : split c s''
            where (w, s'') = break (==c) s' 
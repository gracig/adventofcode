import qualified Data.Map as Map
import Control.Applicative
import System.IO
import System.Environment   

main = do
--    testday2
--    testday5
--    testday7
--    testday9
    testday11

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

data Instruction = Instruction Integer Integer Integer Integer Integer
type Pointer    = Integer
type Value      = Integer
type Program    = Map.Map Pointer Value
data State      = Done 
                | Failed 
                | Output 
                | Input deriving (Eq, Show)

data Status     = Status {state::State, pointer::Pointer, program::Program, refvalue::Value, info::String}
type Input      = [Integer]
type Output     = [Integer]

compute :: Program -> Input -> ( Status, Output)
compute program input = compute' 0 program input []

compute' :: Pointer -> Program -> Input -> Output -> ( Status,Output)
compute' ptr pgm inp out = case computeAt ptr pgm of
    status -> 
        let ptr     = pointer status
            pgr     = program status
            ref     = refvalue status
            getInp  = head inp
            newInp  = tail inp
            newOut  = o:out
            newPgr  = Map.insert newValue getInput p
        in case state status of 
            Done    -> (status, out)
            Failed  -> (status, out)
            Output  -> compute' ptr pgr    inp    newOut
            Input   -> compute' ptr newPgr newInp out

computeAt :: Pointer -> Program -> Status
computeAt ptr pgm = 
        case Map.lookup ptr pgm >>= instructionFromValue >>= opcodeFromInstruction of
        Nothing -> failure "Could not find any operator"
        Just op -> case op of
            let get::Map.Map Integer Integer -> Maybe Integer -> Maybe Integer
                get m k = case (m,k) of
                    (m,Just i)  ->  Map.lookup i m
                    (m,Nothing) ->  Nothing
                g = get pgm
                getValue :: PmMode -> Integer -> Integer
                getValue mode offset = case mode of 
                    Position  -> case g $ g $ fmap (+offset) (Just ptr) of
                        Nothing -> 0
                        Just x  -> x
                    Immediate -> case g $ fmap (+offset) (Just ptr) of 
                        Nothing -> 0
                        Just x -> x
                    Relative  -> case g $ fmap (+getRBase) ( g $ fmap (+offset) (Just ptr) ) of
                        Nothing -> 0
                        Just x -> x
                getRef :: PmMode -> Integer -> Integer
                getRef mode offset = case mode of 
                    Position  -> case g $ fmap (+offset) (Just ptr) of
                        Nothing -> 0
                        Just x -> x
                    Immediate -> case fmap (+offset) (Just ptr) of
                        Nothing -> 0
                        Just x -> x
                    Relative  -> case fmap (+getRBase) ( g $ fmap (+offset) (Just ptr) ) of
                        Nothing -> 0
                        Just x -> x
                setValue :: Pointer -> Value -> Program -> Program
                setValue k v p =  Map.insert k v p
            in Sum m1 m2 m3 -> 
                let p1 = getValue m1 1
                    p2 = getValue m2 2
                    p3 = getRef   m3 3
                    vl = p1 + p2
                    np = ptr + 4
                in  computeAt np ( setValue p3 vl pgm ) 
            Mul m1 m2 m3 ->
                let p1 = getValue m1 1
                    p2 = getValue m2 2
                    p3 = getRef   m3 3
                    vl = p1 * p2
                    np = ptr + 4
                in  computeAt np ( setValue p3 vl pgm ) 
            Lt  m1 m2 m3 ->
                let p1 = getValue m1 1
                    p2 = getValue m2 2
                    p3 = getRef   m3 3
                    vl = if p1 < p2 then 1 else 0
                    np = ptr + 4
                in  computeAt np ( setValue p3 vl pgm ) 
            Eq  m1 m2 m3 ->
                let p1 = getValue m1 1
                    p2 = getValue m2 2
                    p3 = getRef   m3 3
                    vl = if p1 == p2 then 1 else 0
                    np = ptr + 4
                in  computeAt np ( setValue p3 vl pgm ) 
            Jpt m1 m2 ->
                let p1 = getValue m1 1
                    p2 = getValue m2 2
                    np = if p1 /= 0 then p2 else ptr + 3
                in  computeAt np pgm
            Jpf m1 m2 ->
                let p1 = getValue m1 1
                    p2 = getValue m2 2
                    np = if p1 == 0 then p2 else ptr + 3
                in  computeAt np pgm 
            Inp m1 ->
                let p1   = getRef m1 1
                    np = ptr + 2
                in  Status (Input, np, pgm, p1, "Need input") 
            Out m1 ->
                let p1   = getValue m1 1
                    np = ptr + 2
                in  Status (Output, np, pgm, p1, "Just output")
            Rel m1 ->
                let p1   = getValue m1 1
                    np = ptr + 2
                in  computeAt np $ adjRBase p1
            End ->  done

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

split :: Char -> String -> [String]
split c s =  
    case dropWhile (==c) s of
        "" -> []
        s' -> w : split c s''
            where (w, s'') = break (==c) s' 

type Phase = [Integer]
findMax :: Program -> [Phase] -> (Phase, Maybe Integer)
findMax pgm phases = case phases of
    []      ->  error "called with empty list"
    p:[]    ->  (p, amp pgm p)
    p:ps    ->  let ampx = (p, amp pgm p)
                    ampxs = findMax pgm ps
                in  case (>) <$> (snd ampx) <*> (snd ampxs) of
                    Nothing     -> (p, Nothing)
                    Just True   -> ampx
                    Just False  -> ampxs
                                
amp :: Program -> [Integer] -> Maybe Integer
amp pg ps = thruster ( initA . reverse $ ps ) 0
    where
        thruster :: [Status] -> Integer -> Maybe Integer
        thruster chain v = case chainA chain v of
            []              -> Nothing
            (s, i, p, o, _):xs -> case s of
                Output -> case computeAt i p of
                    x@(s',i',p',o', _) -> case s' of
                        Done -> Just o
                        Failed -> Nothing
                        Input -> thruster (x:xs) o
        
        initA :: [Integer] -> [Status]
        initA ps = case ps of
            []      ->  []
            x:xs    ->  case computeAt 0 pg of
                (s, i, p, o, _)    ->  case s of 
                    Input   -> ( computeAt i ( Map.insert o x p) ): (initA xs)
                    _       -> error "init amp failed"
        chainA :: [Status] -> Integer -> [Status]
        chainA as v = case as of
            [] -> []
            x@(s, i, p, o, _):[]   -> case s of
                Input   ->  [ computeAt i ( Map.insert o v p) ]
                _ -> error "Unexpected input"
            x@(s, i, p, o, _):xs   -> case chainA xs v of
                (s', i', p', o', _):ys -> case s' of
                    Output  -> case s of
                        Input -> (computeAt i $ Map.insert o o' p) : (computeAt i' p' ) : ys
                    _ -> error "Unexpected output"
        
findNV :: Map.Map Integer Integer -> Integer -> Integer
findNV map expected =
    head [ 100 * n + v | n <- [0..99], v <- [0..99] , computeNV n v map == Just expected ]

computeNV :: Integer -> Integer -> Program -> Maybe Integer
computeNV n v intcode =  case compute (Map.insert 2 v $ Map.insert 1 n intcode) [] of
    ((s, i, p, o, _), output) -> Map.lookup 0 p

    
runPainterProgram :: Program -> ( Status, [(Char,Char,Integer)])
runPainterProgram pgr = loop 0 pgr [] 0
    where 
        loop :: Pointer -> Program -> [(Char,Char,Integer)] ->Integer ->  (Status, [(Char,Char,Integer)])
        loop ptr pgr answer color = case computeAt ptr pgr of
            status@(s, i, p, o, _) -> case s of 
                Done    -> error "Done"
                Failed  -> error "Failed"
                Output  -> error "Output"
                Input   -> paint i (Map.insert o color p) answer 
        paint :: Pointer -> Program -> [(Char,Char,Integer)] -> (Status)
        

testday11 :: IO()
testday11 = do
    file <- readFile "input"
    let source  = [ read x::Integer | x <- split ',' $  file ]
        intcode = Map.fromList $ zip [0..] source 
    putStrLn $ case paint intcode of 
        ((s, i, p, o, _), output)
            | s /= Done -> "Not finished correctly"
            | otherwise -> unlines $ printOutput output
            where
                printOutput::[(Char,Char,Integer)] -> [String]
                printOutput out = case out of
                    [] -> []
                    (color,direction,steps):xs -> ([color] ++ "-" ++ [direction]) : (printOutput xs)
                                                
testday2 :: IO()
testday2 = do
    file <- readFile "test_day2"
    let source  = [ read x::Integer | x <- split ',' $  file ]
        intcode = Map.fromList $ zip [0..] source 
    putStrLn $ show $ "Day 2. Part 1 " ++  case computeNV 12 2 intcode of 
        Just 4714701 -> "Passed. " ++ show 4714701
        _ -> "Failed"
    putStrLn $ show $ "Day 2. Part 2 " ++  case findNV intcode 19690720 of 
        5121 -> "Passed. " ++ show 5121
        _ -> "Failed"
            
testday5 :: IO()
testday5 = do
    let intcodePEq8 = Map.fromList $ zip [0..] [3,9,8,9,10,9,4,9,99,-1,8]
    putStrLn $ show $ "Day 5. Part 2, Test Position Eq8 " 
        ++ ( expect 1 $ compute intcodePEq8 [8] )
    putStrLn $ show $ "Day 5. Part 2, Test Position Eq8 " 
        ++ ( expect 0 $ compute intcodePEq8 [7] )
    putStrLn $ show $ "Day 5. Part 2, Test Position Eq8 " 
        ++ ( expect 0 $ compute intcodePEq8 [9] )
    let intcodePLt8 = Map.fromList $ zip [0..] [3,9,7,9,10,9,4,9,99,-1,8]
    putStrLn $ show $ "Day 5. Part 2, Test Position Lt8 " 
        ++ ( expect 0 $ compute intcodePLt8 [8] )
    putStrLn $ show $ "Day 5. Part 2, Test Position Lt8 " 
        ++ ( expect 1 $ compute intcodePLt8 [7] )
    putStrLn $ show $ "Day 5. Part 2, Test Position Lt8 " 
        ++ ( expect 0 $ compute intcodePLt8 [9] )
    let intcodeIEq8 = Map.fromList $ zip [0..] [3,3,1108,-1,8,3,4,3,99]
    putStrLn $ show $ "Day 5. Part 2, Test Immediate Eq8 " 
        ++ ( expect 1 $ compute intcodeIEq8 [8] )
    putStrLn $ show $ "Day 5. Part 2, Test Immediate Eq8 " 
        ++ ( expect 0 $ compute intcodeIEq8 [7] )
    putStrLn $ show $ "Day 5. Part 2, Test Immediate Eq8 " 
        ++ ( expect 0 $ compute intcodeIEq8 [9] )
    let intcodeILt8 = Map.fromList $ zip [0..] [3,3,1107,-1,8,3,4,3,99]
    putStrLn $ show $ "Day 5. Part 2, Test Immediate Lt8 " 
        ++ ( expect 0 $ compute intcodeILt8 [8] )
    putStrLn $ show $ "Day 5. Part 2, Test Immediate Lt8 " 
        ++ ( expect 1 $ compute intcodeILt8 [7] )
    putStrLn $ show $ "Day 5. Part 2, Test Immediate Lt8 " 
        ++ ( expect 0 $ compute intcodeILt8 [9] )

    let intcodeLong8 = Map.fromList $ zip [0..] [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
    putStrLn $ show $ "Day 5. Part 2, Long 8 Eq 8" 
        ++ ( expect 1000 $ compute intcodeLong8 [8] )
    putStrLn $ show $ "Day 5. Part 2, Long8 Lt 8 " 
        ++ ( expect 999 $ compute intcodeLong8 [7] )
    putStrLn $ show $ "Day 5. Part 2, Test Gt 8" 
        ++ ( expect 1001 $ compute intcodeLong8 [9] )

    let intcodejump = Map.fromList $ zip [0..] [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]
    putStrLn $ show $ "Day 5. Part 2, Jump.1" 
        ++ ( expect 0 $ compute intcodejump [0] )
    putStrLn $ show $ "Day 5. Part 2, Jump.1" 
        ++ ( expect 1 $ compute intcodejump [1000] )

    let intcodejump2 = Map.fromList $ zip [0..] [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]
    putStrLn $ show $ "Day 5. Part 2, Jump.2" 
        ++ ( expect 0 $ compute intcodejump2 [0] )
    putStrLn $ show $ "Day 5. Part 2, Jump.2" 
        ++ ( expect 1 $ compute intcodejump2 [1000] )        
                
    file <- readFile "test_day5"
    let source  = [ read x::Integer | x <- split ',' $ file ]
        intcode = Map.fromList $ zip [0..] source 
    putStrLn $ show $ "Day 5. Part 1 " 
        ++ ( expect 4511442 $compute intcode [1] )
    putStrLn $ show $ "Day 5. Part 2 " 
        ++ (expect 12648139 $ compute intcode [5] )

testday7 :: IO()
testday7 = do
    let
        seq     =   [0..4]
        phases  =   [ [a,b,c,d,e] | a <- seq, b <- seq, c <- seq, d <- seq, e <- seq
                    , a /= b && a/=c && a/=d && a/=e 
                    , b/= c, b/=d, b/=e
                    , c/=d , c/=e
                    , d/=e
                    ]
        lseq     =   [5..9]
        lphases  =   [ [a,b,c,d,e] | a <- lseq, b <- lseq, c <- lseq, d <- lseq, e <- lseq
                    , a /= b && a/=c && a/=d && a/=e 
                    , b/= c, b/=d, b/=e
                    , c/=d , c/=e
                    , d/=e
                    ]
        code1 = Map.fromList $ zip [0..] [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
        code2 = Map.fromList $ zip [0..] [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0]
        code3 = Map.fromList $ zip [0..] [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]
        code4 = Map.fromList $ zip [0..] [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]
        code5 = Map.fromList $ zip [0..] [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]
    putStrLn $ show $ "Day 7. test. 1 " 
        ++ ( expect' 43210 $ amp code1 [4,3,2,1,0] )
    putStrLn $ show $ "Day 7. test. 1 " 
        ++ ( expect'' ( [4,3,2,1,0], Just 43210 )  $ findMax code1 phases )
    putStrLn $ show $ "Day 7. test. 2 " 
        ++ ( expect' 54321 $ amp code2 [0,1,2,3,4] )
    putStrLn $ show $ "Day 7. test. 2 " 
        ++ ( expect'' ( [0,1,2,3,4], Just 54321 )  $ findMax code2 phases )
    putStrLn $ show $ "Day 7. test. 3 " 
        ++ ( expect' 65210 $ amp code3 [1,0,4,3,2] )
    putStrLn $ show $ "Day 7. test. 3 " 
        ++ ( expect'' ( [1,0,4,3,2], Just 65210 )  $ findMax code3 phases )
    putStrLn $ show $ "Day 7. test. 4 " 
        ++ ( expect' 139629729 $ amp code4 [9,8,7,6,5] )
    putStrLn $ show $ "Day 7. test. 4 " 
        ++ ( expect'' ( [9,8,7,6,5], Just 139629729 )  $ findMax code4 lphases )
    putStrLn $ show $ "Day 7. test. 5 " 
        ++ ( expect' 18216 $ amp code5 [9,7,8,5,6] )
    putStrLn $ show $ "Day 7. test. 5 " 
        ++ ( expect'' ( [9,7,8,5,6], Just 18216 )  $ findMax code5 lphases )
    
    file <- readFile "test_day7"
    let source  = [ read x::Integer | x <- split ',' $ file ]
        intcode = Map.fromList $ zip [0..] source 
    putStrLn $ show $ "Day 7. Part 1 " 
        ++ (expect'' ([2,3,0,4,1],Just 18812) $ findMax intcode phases)
    putStrLn $ show $ "Day 7. Part 2 " 
        ++ (expect'' ([6,9,8,7,5],Just 25534964) $ findMax intcode lphases)


testday9 :: IO()
testday9 = do
    let code1 = Map.fromList $ zip [0..] [109,1,204,(-1),1001,100,1,100,1008,100,16,101,1006,101,0,99]
    putStrLn $ show  $  "Day 9. Test1"  ++ ( show $ compute code1 [1,2..])
    let code2 = Map.fromList $ zip [0..] [1102,34915192,34915192,7,4,7,99,0]
    putStrLn $ show  $  "Day 9. Test2"  ++ ( show $ compute code2 [1,2..])
    let code3 = Map.fromList $ zip [0..] [104,1125899906842624,99]
    putStrLn $ show  $  "Day 9. Test3"  ++ ( show $ compute code3 [1,2..])
    file <- readFile "test_day9"
    let source  = [ read x::Integer | x <- split ',' $  file ]
        intcode = Map.fromList $ zip [0..] source 
    putStrLn $ show  $  "Day 9. Part 1 "  ++ ( expect 2377080455 $ compute intcode [1..])
    putStrLn $ show  $  "Day 9. Part 2 "  ++ ( expect 74917 $ compute intcode [2..])
    
        
expect'' :: (Phase, Maybe Integer) -> (Phase, Maybe Integer) -> String
expect'' e r
    | e == r    = "Passed. " ++ show e
    | otherwise = "Failed. Expected " ++ show e ++ " Got " ++ show r

expect' :: Integer -> Maybe Integer -> String
expect' e r = case r of 
    Nothing -> "has failed computation"
    Just x 
        | x == e   -> "Passed. "++ show e ++ " Got " ++ show x    
        | otherwise     -> "Failed. Expected " ++ show e ++ " Got " ++ show x    
        
expect :: Integer -> (Status, Output) -> String
expect e r = case r of 
    ((Failed, i, p, o,_), out) -> "has failed computation at " ++ show  i
    ((Done, i, p, o,_), output@(x:xs))
        | x == e   -> "Passed. " ++ show output 
        | otherwise     -> "Failed. Expected " ++ show e ++ " Got " ++ show x
            ++ " Output: " ++ show output
    
        

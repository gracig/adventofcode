import qualified IntCode as Intcode
import qualified Image   as Image
import qualified Data.Map as Map
import qualified Data.List as List
import System.IO
import System.Environment  
import Control.Monad
import qualified Control.Monad.Writer as Writer


data Test = Test{ lbl::String
                , fn ::() -> Intcode.State
                , cmp::Intcode.State -> Bool
                }

runExecI::( Intcode.State -> Integer) -> Intcode.State -> (()->Intcode.State)
runExecI inp s = \() -> Intcode.compute $ Intcode.execWithInput inp True s
runExec::Intcode.State -> (()->Intcode.State)
runExec s = \() -> Intcode.compute $ Intcode.exec s
runAmp::Phase -> Intcode.State -> (()->Intcode.State)
runAmp p s = \() -> amp s p
runAmpMax::[Phase] -> Intcode.State -> (()->Intcode.State)
runAmpMax p s = \() -> snd $ findMax s p

runTests::[Test] -> IO()
runTests tests = case tests of
    []   -> return ()
    t:ts -> do
        let s = (fn t) ()
        if (cmp t) s 
            then  putStrLn $ "SUCCESS: " ++ (lbl t) ++" "++ ( show $ reverse (Intcode.acc s))
            else  putStrLn $ "FAILURE: " ++ (lbl t) ++" "++ ( show $ reverse (Intcode.acc s))
        runTests ts

main = do
    day2file <- readFile "test_day2"
    day5file <- readFile "test_day5"
    day7file <- readFile "test_day7"
    day9file <- readFile "test_day9"
    let 
        day2Inp = configNV 12 2 $Intcode.fromList $ zip [0..] [ read x::Integer | x <- split ',' day2file ]
        pgrPEq8 = Intcode.fromList $ zip [0..] [3,9,8,9,10,9,4,9,99,-1,8]
        pgrPLt8 = Intcode.fromList $ zip [0..] [3,9,7,9,10,9,4,9,99,-1,8]
        pgrIEq8 = Intcode.fromList $ zip [0..] [3,3,1108,-1,8,3,4,3,99]
        pgrILt8 = Intcode.fromList $ zip [0..] [3,3,1107,-1,8,3,4,3,99]
        pgrBig8 = Intcode.fromList $ zip [0..] [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
        pgrPJmp = Intcode.fromList $ zip [0..] [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]
        pgrIJmp = Intcode.fromList $ zip [0..] [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]
        day5Inp = Intcode.fromList $ zip [0..] [ read x::Integer | x <- split ',' day5file ]
        phase1 = combo [0..4]
        phase2 = combo [5..9]
        day7tt1 = Intcode.fromList $ zip [0..] [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
        day7tt2 = Intcode.fromList $ zip [0..] [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0]
        day7tt3 = Intcode.fromList $ zip [0..] [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]
        day7tt4 = Intcode.fromList $ zip [0..] [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]
        day7tt5 = Intcode.fromList $ zip [0..] [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]
        day7Inp = Intcode.fromList $ zip [0..] [ read x::Integer | x <- split ',' day7file ]
        day9tt1 = Intcode.fromList $ zip [0..] [109,1,204,(-1),1001,100,1,100,1008,100,16,101,1006,101,0,99]
        day9tt2 = Intcode.fromList $ zip [0..] [1102,34915192,34915192,7,4,7,99,0]
        day9tt3 = Intcode.fromList $ zip [0..]  [104,1125899906842624,99]
        day9Inp = Intcode.fromList $ zip [0..] [ read x::Integer | x <- split ',' day9file ]
        tests = [ Test{ lbl="N=12 v=2 then 4714701"     , fn=runExec day2Inp                    , cmp=cmpValueAt 0 4714701} 
                , Test{ lbl="Position  EQ 8 Then 1"     , fn=runExecI (constant 8) pgrPEq8      , cmp=cmpHeadAcc 1} 
                , Test{ lbl="Position  LT 8 Then 0"     , fn=runExecI (constant 1) pgrPEq8      , cmp=cmpHeadAcc 0} 
                , Test{ lbl="Position  GT 8 Then 0"     , fn=runExecI (constant 9) pgrPEq8      , cmp=cmpHeadAcc 0} 
                , Test{ lbl="Position  EQ 8 Then 0"     , fn=runExecI (constant 8) pgrPLt8      , cmp=cmpHeadAcc 0} 
                , Test{ lbl="Position  LT 8 Then 1"     , fn=runExecI (constant 7) pgrPLt8      , cmp=cmpHeadAcc 1} 
                , Test{ lbl="Position  GT 8 Then 0"     , fn=runExecI (constant 9) pgrPLt8      , cmp=cmpHeadAcc 0}
                , Test{ lbl="Immediate EQ 8 Then 1"     , fn=runExecI (constant 8) pgrIEq8      , cmp=cmpHeadAcc 1} 
                , Test{ lbl="Immediate LT 8 Then 0"     , fn=runExecI (constant 1) pgrIEq8      , cmp=cmpHeadAcc 0} 
                , Test{ lbl="Immediate GT 8 Then 0"     , fn=runExecI (constant 9) pgrIEq8      , cmp=cmpHeadAcc 0} 
                , Test{ lbl="Immediate EQ 8 Then 0"     , fn=runExecI (constant 8) pgrILt8      , cmp=cmpHeadAcc 0} 
                , Test{ lbl="Immediate LT 8 Then 1"     , fn=runExecI (constant 7) pgrILt8      , cmp=cmpHeadAcc 1} 
                , Test{ lbl="Immediate GT 8 Then 0"     , fn=runExecI (constant 9) pgrILt8      , cmp=cmpHeadAcc 0}
                , Test{ lbl="Big EQ 8 Then 1000"        , fn=runExecI (constant 8) pgrBig8      , cmp=cmpHeadAcc 1000} 
                , Test{ lbl="Big LT 8 Then 999"         , fn=runExecI (constant 7) pgrBig8      , cmp=cmpHeadAcc 999} 
                , Test{ lbl="Big GT 8 Then 1001"        , fn=runExecI (constant 9) pgrBig8      , cmp=cmpHeadAcc 1001}
                , Test{ lbl="Position Jump 0 Then 0"    , fn=runExecI (constant 0) pgrPJmp      , cmp=cmpHeadAcc 0}
                , Test{ lbl="Position Jump 1000 Then 1" , fn=runExecI (constant 1000) pgrPJmp   , cmp=cmpHeadAcc 1}
                , Test{ lbl="Immediate Jump 0 Then 0"   , fn=runExecI (constant 0) pgrIJmp      , cmp=cmpHeadAcc 0}
                , Test{ lbl="Immediate Jump 1000 Then 1", fn=runExecI (constant 1000) pgrIJmp   , cmp=cmpHeadAcc 1}
                , Test{ lbl="If 1 Then Expect 4511442"  , fn=runExecI (constant 1) day5Inp      , cmp=cmpHeadAcc 4511442}
                , Test{ lbl="If 5 Then Expect 12648139" , fn=runExecI (constant 5) day5Inp      , cmp=cmpHeadAcc 12648139}
                , Test{ lbl="Day 7. Test1"              , fn=runAmp [4,3,2,1,0] day7tt1         , cmp=cmpHeadAcc 43210}
                , Test{ lbl="Day 7. Test2"              , fn=runAmp [0,1,2,3,4] day7tt2         , cmp=cmpHeadAcc 54321}
                , Test{ lbl="Day 7. Test3"              , fn=runAmp [1,0,4,3,2] day7tt3         , cmp=cmpHeadAcc 65210}
                , Test{ lbl="Day 7. Test4"              , fn=runAmp [9,8,7,6,5] day7tt4         , cmp=cmpHeadAcc 139629729}
                , Test{ lbl="Day 7. Test5"              , fn=runAmp [9,7,8,5,6] day7tt5         , cmp=cmpHeadAcc 18216}
                , Test{ lbl="Day 7. Part1"              , fn=runAmpMax phase1 day7Inp           , cmp=cmpHeadAcc 18812}
                , Test{ lbl="Day 7. Part2"              , fn=runAmpMax phase2 day7Inp           , cmp=cmpHeadAcc 25534964}
                , Test{ lbl="Quine"                     , fn=runExecI (constant 1) day9tt1      , cmp=cmpOutput [109,1,204,(-1),1001,100,1,100,1008,100,16,101,1006,101,0,99]}
                , Test{ lbl="Day 9. Part1"              , fn=runExecI (constant 1) day9Inp      , cmp=cmpHeadAcc 2377080455}
                , Test{ lbl="Day 9. Part2"              , fn=runExecI (constant 2) day9Inp      , cmp=cmpHeadAcc 74917}
                ]
    runTests tests
    exec11

type Phase = [Integer]

findMax :: Intcode.State -> [Phase] -> (Phase, Intcode.State)
findMax pgm phases = case phases of
    []      ->  error "called with empty list"
    p:[]    ->  (p, amp pgm p)
    p:ps    ->  let ampx = (p, amp pgm p)
                    ampxs = findMax pgm ps
                    x1 = head (Intcode.acc $ snd ampx)
                    x2 = head (Intcode.acc $ snd ampxs)
                in  if x1 > x2 
                    then ampx
                    else ampxs

amp :: Intcode.State -> [Integer] -> Intcode.State
amp code phases = Intcode.compute $ Intcode.parInput phases (repeat code) >>= Intcode.execChain 0

ampLog :: Intcode.State -> [Integer] -> (Intcode.State,[String])
ampLog code phases = Intcode.computeWithLog $ Intcode.parInput phases (repeat code)  >>= Intcode.execChain 0

findNV :: Intcode.State -> Integer -> Integer
findNV code expected = 
    head [ 100 * n + v | n <- [0..99], v <- [0..99] , (fst $ computeNV n v code) == Just expected ]

configNV :: Integer -> Integer -> Intcode.State -> Intcode.State
configNV n v s = Intcode.compute $ Intcode.set 2 v s >>= Intcode.set 1 n

computeNV :: Integer -> Integer -> Intcode.State -> (Maybe Integer,[String])
computeNV n v s = Intcode.computeWithLog $  Intcode.exec (configNV n v s) >>= Intcode.get 0

computeIO :: (Intcode.State->Integer) -> Intcode.State  -> (Intcode.State, [String])
computeIO f s = Intcode.computeWithLog $ Intcode.execWithInput f True s

cmpValueAt :: Integer -> Integer -> ( Intcode.State -> Bool)
cmpValueAt k e = \x -> case (Intcode.sts x,Intcode.compute $ Intcode.get k x ) of
    (Intcode.Done,Just x) -> x == e
    (_,_)    -> False

cmpOutput :: [Integer] -> (Intcode.State -> Bool)
cmpOutput e = \x -> case (Intcode.sts x, Intcode.acc x) of
    (Intcode.Done, acc) -> e == reverse acc 

cmpHeadAcc :: Integer -> ( Intcode.State -> Bool)
cmpHeadAcc e = \x -> case (Intcode.sts x,Intcode.acc x) of
    (Intcode.Done,x:xs) -> x == e
    (_,_)    -> False

constant :: Integer -> ( Intcode.State -> Integer)
constant c = \_ -> c

printList :: [String] -> IO ()
printList xs = forM_ xs (\s -> print ("    " ++ s))

split :: Char -> String -> [String]
split c s =  
    case dropWhile (==c) s of
        "" -> []
        s' -> w : split c s''
            where (w, s'') = break (==c) s' 
      
combo :: [Integer] -> [[Integer]]
combo seq =  [ [a,b,c,d,e] | a <- seq, b <- seq, c <- seq, d <- seq, e <- seq
             , a /= b && a/=c && a/=d && a/=e 
             , b/= c, b/=d, b/=e
             , c/=d , c/=e
             , d/=e
             ]

type Point = (Integer,Integer)
type Color = Integer
type Paint = Integer
type Grid  = Map.Map Point Color

data Direction = UP | DOWN | LEFT | RIGHT deriving (Show)
type Robot = (Point,Direction)
data PaintStatus = PaintStatus { robot::Robot, grid::Grid, code::Intcode.State, defaultColor::Integer } deriving (Show)

painter :: PaintStatus -> (PaintStatus,[String])
painter = Writer.runWriter . painter'
    where
        painter' :: PaintStatus -> Writer.Writer [String] PaintStatus
        painter' p = case p of 
            PaintStatus robot@((rx,ry),rd) grid code defaultColor-> case Intcode.sts code of
                Intcode.Begin -> 
                    let code' = Intcode.compute $ Intcode.exec code
                    in  do
                    Writer.tell [ "BEGIN Robot: " ++ show robot ++ "Grid: " ++ show grid  ] 
                    painter' $ PaintStatus robot grid code' defaultColor
                Intcode.Input ->
                    let color    = view defaultColor robot grid
                        code'    = Intcode.compute $ Intcode.input color code >>= Intcode.exec
                        (t:c:xs) = Intcode.acc code'
                        grid'    = paint c robot grid
                        robot'   = ( (move 1) . (turn t) ) robot
                    in  do
                        Writer.tell [ "Input: " ++ show color++ " Paint: " ++ show (fst robot) ++" "++ show c ++  " New Position: " ++ show robot'  ] 
                        painter' $ PaintStatus robot' grid' code' defaultColor
                _ -> return p
            where
                turn :: Integer -> Robot -> Robot
                turn x r = case r of
                    ((rx,ry),d) -> ((rx,ry),turn' x d)
                turn':: Integer -> Direction -> Direction
                turn' x d = case (x,d) of
                    (0,UP) -> LEFT
                    (0,DOWN) -> RIGHT
                    (0,LEFT) -> DOWN
                    (0,RIGHT) -> UP
                    (1,UP) -> RIGHT
                    (1,DOWN) -> LEFT
                    (1,LEFT) -> UP
                    (1,RIGHT) -> DOWN
                    (x',d') -> error ((show x') ++  "wrong" ++ (show d'))
                move steps robot = case (steps,robot) of
                    (_,((x,y),UP))    -> ( (x,y+steps), UP   )
                    (_,((x,y),DOWN))  -> ( (x,y-steps), DOWN )
                    (_,((x,y),LEFT))  -> ( (x-steps,y), LEFT )
                    (_,((x,y),RIGHT)) -> ( (x+steps,y), RIGHT)
                paint::Integer -> Robot -> Grid -> Grid
                paint color robot grid = case robot of
                    ( xy ,_) -> Map.insert xy color grid
                view::Integer -> Robot -> Grid -> Color
                view defaultColor robot grid = case robot of 
                    ( xy ,_) -> case Map.lookup xy grid of
                        Nothing -> defaultColor
                        Just x  -> x


gridBounds :: Grid -> (Integer,Integer,Integer,Integer)
gridBounds grid = Map.foldlWithKey  (\(xmin,xmax,ymin,ymax) (x,y) c -> 
                                        let xmin' = if x<xmin then x else xmin
                                            xmax' = if x>xmax then x else xmax
                                            ymin' = if y<ymin then y else ymin
                                            ymax' = if y>ymax then y else ymax
                                        in  (xmin',xmax',ymin',ymax')
                                    ) (0,0,0,0) grid

encodeAsImageLayer :: Grid -> Image.Layer
encodeAsImageLayer grid = 
    let (xmin,xmax,ymin,ymax) = gridBounds grid
        xlen = xmax-xmin+1
        ylen = ymax-ymin+1
        len  = xlen * ylen
        txy  = \x y -> (x-xmin) + ( xlen * ((ylen-1)- (y-ymin)) )
        trg  = Map.foldlWithKey (\trg (x,y) c -> Map.insert (txy x y) c trg )  (Map.fromList [ (x, 0) | x <- [0..len-1] ]) grid
        raw  = map snd $ Map.toList trg
    in  Image.decode $ Image.fromList (fromIntegral xlen) (fromIntegral ylen) raw
            
exec11::IO()
exec11 = do
    input11 <- readFile "input_11"
    let
        robot = ((0,0),UP)
        code = Intcode.fromList $ zip [0..] [ read x::Integer | x <- split ',' input11 ]
        (paintStatus,log) = painter $ PaintStatus robot (Map.fromList []) code 0
        panel = grid paintStatus
        layer = encodeAsImageLayer panel
        (paintStatus',log') = painter $ PaintStatus robot (Map.fromList [((0,0),1)]) code 0
        panel' = grid paintStatus'
        bounds' = gridBounds panel'
        layer' = encodeAsImageLayer panel'
    putStrLn $ show $ Map.size panel
    putStrLn $ show $ Map.size panel'
    putStrLn $ show bounds'
    Image.printLayer layer'

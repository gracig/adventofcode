import qualified Data.Map as Map
import qualified Data.List as List
import Control.Applicative
import Control.Monad

type Point     = (Float,Float)
type Asteroids = [Point]

main = do
    
--    test1
--    test2
--    test3
--    test4
--    test5
--    part1
    test6

printList ::(Show a) => [a] -> IO ()
printList xs = forM_ xs (\s -> print s)

isBetween :: Point -> Point -> Point -> Bool
isBetween (ax,ay) (bx,by) (cx,cy) = 
    let
        xprod       = (cy - ay) * (bx - ax) - (cx - ax) * (by - ay)
        dprod       = (cx - ax) * (bx - ax) + (cy - ay) * (by - ay)
        sqLenba     = (bx - ax) * (bx - ax) + (by - ay) * (by - ay)
        isAlign     = not (( abs xprod ) > 0.0)
        isDPositive = dprod >= 0
        isDLtSqLen  = dprod <= sqLenba
    in isAlign && isDPositive && isDLtSqLen

isOnLine :: Point -> Point -> Point -> Bool
isOnLine (x1,y1) (x2,y2) (x3,y3) = 
    let
        m = ((y2)-(y1))/(x2-x1)
        c =   (y1) - m * x1
    in  (y3) == m * x3 + c 

moreSights :: [(Point,Integer)] -> (Point,Integer)
moreSights = foldl1 (\a@(x1,y1) b@(x2,y2) -> if y1 > y2 then a else b)

sights :: Asteroids -> [(Point,Integer)]
sights asteroids = Map.toList $ sights' [ (a,b) | a <- asteroids, b <- asteroids , a/=b ]
        where
            sights' line = case line of
                [] -> Map.empty
                (a,b):xs -> 
                    let 
                        asteroids'  = [ x| x<-asteroids, x/=a, x/=b]
                        isDirect    = foldl (\acc c -> acc && not (isBetween a b c) ) True asteroids' 
                        value       = if isDirect then 1 else 0
                    in
                        Map.insertWith (+) a value (sights' xs)
    
fromList :: [String] -> Asteroids
fromList list = fromList' list 0
    where 
        fromList' :: [String] -> Float -> Asteroids
        fromList' list cy = case list of
            [] -> []
            x:xs -> foldl ( \acc (cx,v) -> if v == '#' then (cx,cy):acc else acc ) [] ( zip [0..] x ) 
                    ++ fromList' xs (cy+1)

test1 :: IO()
test1 = do
    contents <- readFile "test1"
    putStrLn $  show . moreSights . sights .fromList . lines $ contents
                    
test2 :: IO()
test2 = do
    contents <- readFile "test2"
    putStrLn $  show . moreSights . sights .fromList . lines $ contents

test3 :: IO()
test3 = do
    contents <- readFile "test3"
    putStrLn $  show . moreSights . sights .fromList . lines $ contents

test4 :: IO()
test4 = do
    contents <- readFile "test4"
    putStrLn $  show . moreSights . sights .fromList . lines $ contents

test5 :: IO()
test5 = do
    contents <- readFile "test5"
    putStrLn $  show . moreSights . sights .fromList . lines $ contents

test6 :: IO()
test6 = do
    contents <- readFile "test6"
    printList . sights .fromList . lines $ contents
    putStrLn $  show . moreSights . sights .fromList . lines $ contents
        

part1 :: IO()
part1 = do
    contents <- readFile "input"
    putStrLn $  show . moreSights . sights .fromList . lines $ contents
    
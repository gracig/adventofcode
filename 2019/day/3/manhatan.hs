import qualified Data.List as List
import Control.Applicative
main = do
   input1 <- getLine
   input2 <- getLine
   let
       origin = (0,0)
       path1  = fromDirections origin [ x | x <- split ',' $ init input1 ]
       path2  = fromDirections origin [ x | x <- split ',' $ init input2 ]
       intersections = intersectionPath path1 path2
   putStrLn $ "Input1 " ++ input1
   putStrLn $ "Path1 " ++ (show path1)
   putStrLn $ "Input2 " ++ input2
   putStrLn $ "Path2 " ++ (show path2)
   putStrLn $ "Intersections at " ++ ( show intersections) 
   putStrLn $ "Closest intersection distance: " ++ (show $ closestPointDistance origin intersections)
   putStrLn $ "Lowest steps intersection distance: " ++ (show $ lowestStepsDistance path1 path2 intersections)

type Direction = String
type Point     = (Integer,Integer)
type Path      = [ Point ]
type Segment   = (Point,Point)

fromDirections :: Point -> [Direction] -> Path
fromDirections p ds =  
    reverse $ foldl (\acc d -> nextPoint (head acc) d : acc ) [p] ds
    where
        nextPoint :: Point -> Direction -> Point
        nextPoint (x1,y1) (d:num)
            | isUp = (x1,y1+step)
            | isDown = (x1,y1-step)
            | isLeft = (x1-step,y1)
            | isRight = (x1+step,y1)
            | otherwise = error "Not recognized direction"
            where
                isUp    = d == 'U'
                isDown  = d == 'D'
                isLeft  = d == 'L'
                isRight = d == 'R'
                step    = read num::Integer

closestPointDistance:: Point -> [Point] -> Integer
closestPointDistance p ps =
    distance p $ closest p $ filter (/=p) ps 
    
closest:: Point -> [ Point ] -> Point
closest o [] = error "closest using empty list"
closest o (p:[]) = p
closest o (p:ps) 
    | (distance o p) < (distance o minTail) = p
    | otherwise = minTail
    where
        minTail = closest o ps

distance::Point -> Point -> Integer
distance (p1,p2) (q1,q2) = abs(p1 - q1) + abs(p2 -q2)

intersectionPath::Path -> Path -> [Point]
intersectionPath a b =
    [ iab  | la <- zip a (tail a), lb <- zip b (tail b), iab <- intersectionSegment la lb ]

lowestStepsDistance:: Path -> Path -> [Point] -> Maybe Integer
lowestStepsDistance a b ps = 
    case lowestSteps a b ps of
        Nothing -> Nothing
        (Just x) -> combinedStepsToPoint a b x 

lowestSteps :: Path -> Path -> [Point] -> Maybe Point
lowestSteps _ _ []     = Nothing
lowestSteps a b (p:[]) = Just p
lowestSteps a b (p:ps) 
    | isLowest = Just p
    | otherwise = lowestTail
    where
        fromMaybe::Maybe Point -> Point
        fromMaybe Nothing  = error "point not found"
        fromMaybe (Just p) = p
        lowestTail = lowestSteps a b ps
        tailSteps = combinedStepsToPoint  a b $ fromMaybe lowestTail
        steps = combinedStepsToPoint a b p 
        isLowest = ((<) <$> steps <*> tailSteps) == Just True 

combinedStepsToPoint :: Path -> Path -> Point -> Maybe Integer
combinedStepsToPoint a b p =
    (+) <$> pathStepsToPoint p a <*> pathStepsToPoint p b 

pathStepsToPoint:: Point -> Path -> Maybe Integer
pathStepsToPoint _ []     = Nothing
pathStepsToPoint _ (_:[]) = Nothing
pathStepsToPoint x (p:ps) =
    if isBetween x (p, head ps)
        then Just (distance p x)
        else (+) <$> Just (distance p (head ps))  <*>  pathStepsToPoint x ps 

intersectionSegment::Segment -> Segment -> [Point]
intersectionSegment ((x1,y1), (x2,y2)) ((x3,y3),(x4,y4)) =
    let 
        dx12 = x1 - x2
        dx34 = x3 - x4
        dy12 = y1 - y2
        dy34 = y3 - y4
        den  = dx12 * dy34 - dy12 * dx34
    in
        if den == 0
            then []
        else
            let
                det12 = x1*y2 - y1*x2
                det34 = x3*y4 - y3*x4
                numx  = det12 * dx34 - dx12 * det34
                numy  = det12 * dy34 - dy12 * det34
            in
                [ p | p  <-[ ( numx `div` den, numy `div` den) ]
                , isBetween p ((x1,y1), (x2,y2))
                , isBetween p ((x3,y3), (x4,y4))
                ]

isBetween::Point -> Segment -> Bool
isBetween (cx, cy) ((ax,ay), (bx,by))
    | abs(crossproduct) /= 0 = False
    | dotproduct < 0 = False
    | dotproduct > squaredlengthba = False
    | otherwise = True
    where
        crossproduct    = (cy - ay) * (bx - ax) - (cx - ax) * (by - ay)
        dotproduct      = (cx - ax) * (bx - ax) + (cy - ay) * (by - ay)
        squaredlengthba = (bx - ax) *(bx - ax) + (by - ay)*(by - ay)

split :: Char -> String -> [String]
split c s =  
    case dropWhile (==c) s of
        "" -> []
        s' -> w : split c s''
            where (w, s'') = break (==c) s'
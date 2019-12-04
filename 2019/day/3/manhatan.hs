import qualified Data.List as List
import Control.Applicative

type Direction = String
type Point     = (Integer,Integer)
type Path      = [ Point ]
type Segment   = (Point,Point)

main = do
   input1 <- getLine
   input2 <- getLine
   let
       origin = (0,0)
       path1  = fromDirections origin [ x | x <- split ',' $ init input1 ]
       path2  = fromDirections origin [ x | x <- split ',' $ init input2 ]
       intersections = intersectionPath path1 path2
   putStrLn $ "Intersections at " ++ ( show intersections) 
   putStrLn $ "Closest intersection distance: " ++ (show $ closestPointDistance origin intersections)
   putStrLn $ "Lowest steps intersection distance: " ++ (show $ lowestStepsDistance path1 path2 intersections)

fromDirections :: Point -> [Direction] -> Path
fromDirections p ds =  
    reverse $ foldl appendPoint [p] ds
    where
        appendPoint :: Path -> Direction -> Path
        appendPoint ps d = case ps of
            []  ->  []
            ps  ->  case nextPoint (head ps) d of
                        Nothing -> []
                        Just p  -> p:ps 
        nextPoint :: Point -> Direction -> Maybe Point
        nextPoint (x1,y1) (d:num)
            | isUp      = Just (x1,y1+step)
            | isDown    = Just (x1,y1-step)
            | isLeft    = Just (x1-step,y1)
            | isRight   = Just (x1+step,y1)
            | otherwise = Nothing
            where
                isUp    = d == 'U'
                isDown  = d == 'D'
                isLeft  = d == 'L'
                isRight = d == 'R'
                step    = read num::Integer

closestPointDistance :: Point -> [Point] -> Maybe Integer
closestPointDistance p ps = case closest p $ filter (/=p) ps  of
    Nothing -> Nothing
    Just x  -> Just (distance p x)
    
closest :: Point -> [ Point ] -> Maybe Point
closest o xs = case (o,xs) of
    (o,[]  )    ->  Nothing
    (o,p:[])    ->  Just p
    (o,p:ps)    |   isCloser == Just True  -> Just p
                |   isCloser == Nothing    -> Nothing
                |   otherwise -> minTail
                where 
                    minTail   = closest o ps
                    isCloser = do
                        mint <- minTail
                        return $ (distance o p) < (distance o mint) 

distance::Point -> Point -> Integer
distance (p1,p2) (q1,q2) =
    abs(p1 - q1) + abs(p2 -q2)

intersectionPath::Path -> Path -> [Point]
intersectionPath a b =
    [ iab  | la <- zip a (tail a), lb <- zip b (tail b), iab <- intersectionSegment la lb ]

lowestStepsDistance:: Path -> Path -> [Point] -> Maybe Integer
lowestStepsDistance a b ps = 
    case lowestSteps a b ps of
        Nothing -> Nothing
        (Just x) -> combinedStepsToPoint a b x 

lowestSteps :: Path -> Path -> [Point] -> Maybe Point
lowestSteps a b ps = case (a,b,ps) of
    (_,_,[])    ->  Nothing
    (a,b,p:[])  ->  Just p
    (a,b,p:ps)  |   isLowest  -> Just p
                |   otherwise -> lowestTail
                where
                    lowestTail = lowestSteps a b ps
                    tailSteps  = lowestTail >>= combinedStepsToPoint  a b
                    steps      = combinedStepsToPoint a b p 
                    isLowest   = ((<) <$> steps <*> tailSteps) == Just True 

combinedStepsToPoint :: Path -> Path -> Point -> Maybe Integer
combinedStepsToPoint a b p =
    (+) <$> pathStepsToPoint p a <*> pathStepsToPoint p b 

pathStepsToPoint :: Point -> Path -> Maybe Integer
pathStepsToPoint x ps = case ps of
    []      ->  Nothing
    _:[]    ->  Nothing
    p:ps    ->  if isBetween x (p, head ps)
                    then Just (distance p x)
                    else (+) <$> Just(distance p (head ps)) <*> pathStepsToPoint x ps 

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
        squaredlengthba = (bx - ax) * (bx - ax) + (by - ay) * (by - ay)

split :: Char -> String -> [String]
split c s =  
    case dropWhile (==c) s of
        "" -> []
        s' -> w : split c s''
            where (w, s'') = break (==c) s'
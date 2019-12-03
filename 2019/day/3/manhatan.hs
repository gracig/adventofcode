import qualified Data.List.Split as Split


main = do
   input1 <- getLine
   input2 <- getLine
   let
       path1 = pathFromDirections [ x | x <- Split.splitOn "," $ init input1 ]
       path2 = pathFromDirections [ x | x <- Split.splitOn "," $ init input2 ]
   putStr $ show path1
   putStr $ show path2


type Direction = String
type Point = (Integer,Integer)
type Line  = (Point,Point)
type Path  = [ Line ]

-- pathFromDirections::Point -> [Direction] -> Point -> Path
-- pathFromDirections (d:ds) p  = 

lineFromDirection::Direction -> Point -> Line
lineFromDirection (d:num) (x1,y1)
    | isUp = ((x1,y1),(x1,y1+step)
    | isDown = ((x1,y1),(x1,y1-step)
    | isLeft = ((x1,y1),(x1-step,y1)
    | isRight = ((x1,y1),(x1+step,y1)
    | otherwise = error "Not recognized direction"
    where
        isUp    = d == "U"
        isDown  = d == "D"
        isLeft  = d == "L"
        isRight = d == "R"
        step    = read num::Integer


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
intersectionPath a b=
    [ iab  | la <- a, lb <- b, iab <- intersectionLine la lb ]

intersectionLine::Line -> Line -> [Point]
intersectionLine ((x1,y1), (x2,y2)) ((x3,y3),(x4,y4)) =
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
                [ ( numx `div` den, numy `div` den) ]

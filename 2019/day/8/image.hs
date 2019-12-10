import Control.Applicative
import System.IO
import System.Environment   

main = do
    testday2

type Row = [Integer]
type Layer = [Row]
type Image = [Layer]

layerWithFewestZeroes :: Image -> Layer
layerWithFewestZeroes = foldr1 (\x acc -> if (lzeroes x) < (lzeroes acc) then x else acc)

lzeroes :: Layer -> Integer
lzeroes l = lcount l 0

lone :: Layer -> Integer
lone l = lcount l 1

ltwo :: Layer -> Integer
ltwo l = lcount l 2

lcount :: Layer -> Integer -> Integer
lcount l d = case l of 
    []      -> 0
    r:rs    -> (rcount r d) + (lcount rs d)

rcount :: Row -> Integer -> Integer
rcount r d = case r of
    []      -> 0
    x:xs
        | x == d -> 1 + ( rcount xs d )
        | otherwise -> rcount xs d
    
imageFromList :: Int -> Int -> [Integer] -> Image
imageFromList w h pixels
    | isEmpty = []
    | isValid = layerFromList w (take (w*h) pixels ) : imageFromList w h ( drop (w*h) pixels )
    | otherwise = error "image - invalid number of pixels"
    where
        size = length pixels
        isEmpty = size==0
        isValid = size >= w*h

layerFromList :: Int -> [Integer] -> Layer
layerFromList w pixels
    | isEmpty   = []
    | isValid   = take w pixels : layerFromList w ( drop w pixels ) 
    | otherwise = error "layer - invalid number of pixels"
    where
        size = length pixels
        isEmpty = size == 0
        isValid = size >= w

decode :: Image -> Layer
decode image = case image of
    []      -> error "Image has no layers"
    l:[]    -> l
    l:ls    ->   zipWith (\a b ->  zipWith (\c d  -> if c == 2 then d else c)  a b   )  l (decode ls)

testday2 :: IO()
testday2 = do
    contents <- readFile "input"
    let source  = [ read [x]::Integer  | x <- contents ]
        encoded = imageFromList 25 6 source
        fewest =   layerWithFewestZeroes encoded
        zeroes = lzeroes fewest
        ones = lone fewest
        twos = ltwo fewest
        result = ones * twos
        image = decode encoded
        
    putStrLn $ show ("Zeroes " ++ show zeroes  ++ " Ones "++ show ones ++ " Twos " ++ show twos) 
    printLayer image

printLayer :: Layer -> IO ()    
printLayer l = case l of 
    [] -> return ()
    r:rs -> do 
        printPixels r
        printLayer rs

printPixels :: Row -> IO ()    
printPixels row = case row of 
    [] -> putStrLn ""
    x:xs -> do
        putStr $ case x of 
            1 -> "X"
            _ -> " "
        printPixels xs
        
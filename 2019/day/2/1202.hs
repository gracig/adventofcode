import qualified Data.Map as Map
import Control.Applicative

main = do
    contents <- getContents
    let source  = [ read x::Integer | x <- split ',' $ init contents ]
        intcode = Map.fromList $ zip [0..] source 
        result  = run 12 2 intcode
    putStrLn $ "Initial IntCode: " ++ (show intcode)
    putStrLn $ "Final   IntCode: " ++ (show result)
    putStrLn $ "The value at position 0 is " ++ (show $ Map.lookup 0 result)
    let nvresult = findNounAndVerb intcode 19690720
    putStrLn $ "The value of nvresult is " ++ (show nvresult )

split :: Char -> String -> [String]
split c s =  
    case dropWhile (==c) s of
        "" -> []
        s' -> w : split c s''
            where (w, s'') = break (==c) s'

-- Stage 2: Find noun and verbs that satisfies a criteria
findNounAndVerb :: Map.Map Integer Integer -> Integer -> Integer
findNounAndVerb map expected =
    head [ 100 * n + v | n <- [0..99], v <- [0..99] , result n v == Just expected ]
    where result n v = Map.lookup 0 $ run n v map

--Stage 1: compute intcode instructions and return another modified intcode
run :: Integer -> Integer -> Map.Map Integer Integer -> Map.Map Integer Integer
run n v intcode = 
    compute $ Map.insert 2 v $ Map.insert 1 n intcode

compute :: Map.Map Integer Integer -> Map.Map Integer Integer
compute = computeAtIndex (Just 0)

computeAtIndex :: Maybe Integer -> Map.Map Integer Integer -> Map.Map Integer Integer
computeAtIndex idx map 
    | isSum = set map op3 $ (+) <$> op1 <*> op2  
    | isMul = set map op3 $ (*) <$> op1 <*> op2
    | isRet = map
    | otherwise = map
    where 
        get::Map.Map Integer Integer -> Maybe Integer -> Maybe Integer
        get m k = case (m,k) of
            (m,Just i)  ->  Map.lookup i m
            (m,Nothing) ->  Nothing
        set::Map.Map Integer Integer -> Maybe Integer -> Maybe Integer -> Map.Map Integer Integer
        set m k v = case (m,k,v) of
            (m,Nothing,_)       ->  m
            (m,_,Nothing)       ->  m
            -- sets the value and call the next computation
            (m,Just k,Just v)   ->  computeAtIndex next $ Map.insert k v m
        g = get map -- partial application of get map
        isSum = g idx == Just 1
        isMul = g idx == Just 2
        isRet = g idx == Just 99
        op1   = g $ g $ fmap (+1) idx
        op2   = g $ g $ fmap (+2) idx
        op3   = g $ fmap (+3) idx
        next  = fmap (+4) idx
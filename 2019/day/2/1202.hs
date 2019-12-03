import qualified Data.Map as Map
import qualified Data.List.Split as Split
import Control.Applicative

main = do
    contents <- getContents
    let source  = [ read x::Integer | x <-Split.splitOn "," $ init contents ]
        intcode = Map.insert 2 2 $ Map.insert 1 12 $ Map.fromList $ zip [0..] source 
        result  = compute intcode
    putStrLn $ "Initial IntCode: " ++ (show intcode)
    putStrLn $ "Final   IntCode: " ++ (show result)
    putStrLn $ "The value at position 0 is " ++ (show $ Map.lookup 0 result)
    let nvresult = findNounAndVerb intcode 19690720
    putStrLn $ "The value of nvresult is " ++ (show nvresult )

-- Stage 2: Find noun and verbs that satisfies a criteria
findNounAndVerb :: Map.Map Integer Integer -> Integer -> Integer
findNounAndVerb map expected =
    head [ 100 * n + v | n <- [0..99], v <- [0..99] , result n v == Just expected ]
    where result n v = Map.lookup 0 $ compute $ Map.insert 2 v $ Map.insert 1 n map

--Stage 1: compute intcode instructions and return another modified intcode
compute :: Map.Map Integer Integer -> Map.Map Integer Integer
compute = computeAtIndex (Just 0)

computeAtIndex :: Maybe Integer -> Map.Map Integer Integer -> Map.Map Integer Integer
computeAtIndex idx map 
    | isSum = set map op3 $ (+) <$> op1 <*> op2  
    | isMul = set map op3 $ (*) <$> op1 <*> op2
    | isRet = map
    | otherwise = map
    where 
        -- get map values
        get::Map.Map Integer Integer -> Maybe Integer -> Maybe Integer
        get m (Just i) = Map.lookup i m
        get m Nothing = Nothing
        g = get map
        -- opcode predicates
        isSum = g idx == Just 1
        isMul = g idx == Just 2
        isRet = g idx == Just 99
        -- operator values
        op1   = g $ g $ fmap (+1) idx
        op2   = g $ g $ fmap (+2) idx
        op3   = g $ fmap (+3) idx
        opn   = fmap (+4) idx
        -- set map and compute next
        set::Map.Map Integer Integer -> Maybe Integer -> Maybe Integer -> Map.Map Integer Integer
        set m Nothing _ = m
        set m _ Nothing =  m
        set m (Just k) (Just v) = computeAtIndex opn (Map.insert k v m) -- set the value and call the next computation

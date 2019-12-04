main = do
    let range = [273025..767253]
    putStrLn $ "Part 1 answer: " ++ ( show $ length $ findPossiblePasswords range )
    putStrLn $ "Part 2 answer: " ++ ( show $ length $ findPossiblePasswords' range )

findPossiblePasswords::[Integer] -> [Integer]
findPossiblePasswords range =
    [ x | x <- range
    , let ds = digits x
    , length ds == 6
    , isDoubleAdjacent ds
    , isNeverDecrease ds
    ]

findPossiblePasswords'::[Integer] -> [Integer]
findPossiblePasswords' range =
    [ x | x <- range
    , let ds = digits x
    , length ds == 6
    , isDoubleAdjacent' ds
    , isNeverDecrease ds
    ]

digits :: Integral x => x -> [x]
digits x = case x of
    0       ->  []
    x       ->  digits (x `div` 10) ++ [ x `mod` 10 ]
        
isNeverDecrease :: [Integer] -> Bool
isNeverDecrease xs = case xs of
    []      ->  False
    (x:[])  ->  True
    (x:xs)  ->  if x > (head xs)
                    then False
                    else isNeverDecrease xs
    
isDoubleAdjacent :: [Integer] -> Bool
isDoubleAdjacent xs = case xs of
    []     ->   False
    [x]    ->   False
    (x:xs) ->   if x == (head xs)
                    then True
                    else isDoubleAdjacent xs

isDoubleAdjacent' :: [Integer] -> Bool
isDoubleAdjacent' xs = case xs of
    []          ->  False
    (x:[])      ->  False
    (x1:x2:[])  ->  x1 == x2
    (x:xs)      ->  if (x == (head xs)) 
                        then if (x /= (head (tail xs)))
                            then True
                            else isDoubleAdjacent' $ dropWhile (==x) xs
                        else isDoubleAdjacent' xs

hasNDigits::Integer -> Integer -> Bool
hasNDigits digits value
    | r > 0 && r <10 = True
    | otherwise      = False
    where
        r = div value ( 10^(digits-1) )
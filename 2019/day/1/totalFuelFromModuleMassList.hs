main = do
    contents <- getContents 
    let total = totalFuel [ read x::Integer | x <- lines contents ]
    putStrLn $ (++) "O resultado é: " $ show total

moduleFuel::Integer->Integer
moduleFuel x = 
    (x `div` 3) - 2

totalFuel::[Integer]->Integer
totalFuel xs = 
    foldr ( \x acc ->  moduleFuel(x) + acc ) 0 xs

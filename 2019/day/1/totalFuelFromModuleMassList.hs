
main = do
    contents <- getContents 
    let modules = [ read x::Integer | x <- lines contents ] -- this precious line convert the
                                                            -- strings from the input file into
                                                            -- a list of integers
        total1  = totalModuleFuel modules
        total2  = totalRequiredFuel modules
    putStrLn $ (++) "1) The sum of the fuel requirements is: " $ show total1
    putStrLn $ (++) "2) The sum of the fuel requirements is: " $ show total2

-- moduleFuel calculates the required fuel of a mass value
moduleFuel::Integer->Integer
moduleFuel mass = (mass `div` 3) - 2

-- calculates the totalModuleFuel using foldr
totalModuleFuel::[Integer]->Integer
totalModuleFuel = foldr ( \x acc ->  moduleFuel(x) + acc ) 0

-- recursively calculates the total requiredFuel for a mass value
-- using guards and where statements
requiredFuel::Integer->Integer
requiredFuel  mass
    | required <= 0 = 0
    | otherwise = required + requiredFuel required
    where required = moduleFuel mass

-- calculates total RequiredFuel using foldr
totalRequiredFuel::[Integer]->Integer
totalRequiredFuel= foldr ( \x acc ->  requiredFuel(x) + acc ) 0
 



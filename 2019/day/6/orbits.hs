import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Applicative
import System.IO
import System.Environment   
import Data.Traversable

type Object     = String
type OrbitMap   = Map.Map Object Object
type OrbitGraph = Map.Map Object [Object]

main = do
    [f] <- getArgs
    contents <- readFile f
    let input  = lines contents
        map = Map.fromList [ (p, o) | l <- input, let [o,p] = split ')' l ]
        graphInput = [ (p, [o]) | l <- input, let [o,p] = split ')' l ]
        graphInput' = [ (o, [p]) | l <- input, let [o,p] = split ')' l ]
        graph = Map.fromListWith (++) ( graphInput ++ graphInput')
    putStrLn $ "The total number of direct and indirect orbits: " ++ (show $ totalOrbits map)
    putStrLn $ "Minimum number of orbital transfers required to move from the object YOU are orbiting to the object SAN is orbiting: " 
                ++ (show  $ minTransferAB "YOU" "SAN" graph)

orbits :: OrbitMap -> Object -> Integer
orbits m o = case Map.lookup o m of
    Nothing -> 0
    Just p  -> 1 + ( orbits m p )

totalOrbits :: OrbitMap -> Integer
totalOrbits map = fst $ mapAccumL (\acc x -> let c=(orbits map x)+1 in (acc+c,c)) 0 map 

minTransferAB:: Object -> Object -> OrbitGraph -> Integer
minTransferAB a b g = case (minTransferAB' a Set.empty) - 3 of
    x   | x<=0      -> 0
        | otherwise -> x
    where
        minTransferAB' :: Object -> Set.Set Object -> Integer
        minTransferAB' k s = case Set.member k s of
            True    ->  0
            False   
                |   k == b    -> 1
                |   otherwise -> case Map.lookup k g of
                        Nothing ->  0
                        Just [] ->  0
                        Just xs ->  case let s' = Set.insert k s in foldl (\acc x -> max (minTransferAB' x s') acc ) 0 xs of
                                        0 -> 0
                                        x -> x + 1

split :: Char -> String -> [String]
split c s =  
    case dropWhile (==c) s of
        "" -> []
        s' -> w : split c s''
            where (w, s'') = break (==c) s' 

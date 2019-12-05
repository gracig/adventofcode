import qualified Data.Map as Map
import Control.Applicative

main = do
    contents <- getContents
    let source  = [ read x::Integer | x <- split ',' $ init contents ]
    putStrLn $ show source

type Index  = Integer
type Value  = Integer
data PmMode = Position | Immediate
data Opcode = Sum Index PmMode PmMode PmMode
            | Mul Index PmMode PmMode PmMode
            | Inp Index PmMode
            | Ret Index PmMode
            | Nop Index
            | End Index
type Memory = Map.Map Index Value
data Instruction = Instruction Index Integer Integer Integer Integer Integer

instructionFromValue :: Index -> Value -> Maybe Instruction
instructionFromValue i v = case digits v of
    []          -> Nothing 
    [e]         -> Just ( Instruction i 0 0 0 0 e )
    [d,e]       -> Just ( Instruction i 0 0 0 d e )
    [c,d,e]     -> Just ( Instruction i 0 0 c d e )
    [b,c,d,e]   -> Just ( Instruction i 0 b c d e )
    [a,b,c,d,e] -> Just ( Instruction i a b c d e )
    xs          -> Nothing

opcodeFromInstruction :: Instruction -> Maybe Opcode
opcodeFromInstruction instruction = case instruction of
    Instruction i a b c d e 
       | (pmode a) == Nothing -> Nothing
       | (pmode b) == Nothing -> Nothing
       | (pmode c) == Nothing -> Nothing
       | opcode  == 0  -> Just (Nop i )
       | opcode  == 1  -> Just (Sum i pc pb pa)
       | opcode  == 2  -> Just (Mul i pc pb pa)
       | opcode  == 3  -> Just (Inp i pc)
       | opcode  == 4  -> Just (Ret i pc)
       | opcode  == 99 -> Just (End i )
       | otherwise   -> Nothing
       where
           opcode = d*10 + e
           pmode x = case x of
               0 -> Just Position
               1 -> Just Immediate
               x -> Nothing
           rpmode :: Integer -> PmMode
           rpmode x = case pmode x of
             Nothing  -> Position 
             Just x  -> x 
           pa = rpmode a 
           pb = rpmode b 
           pc = rpmode c
         
-- readInstruction:: Memory -> dex -> Opcode
-- readInstruction m i = case Map.lookup i m of
--     Nothing -> Err i
--     Just v  -> digits x



digits :: Integral x => x -> [x]
digits x = case x of
    0       ->  []
    x       ->  digits (x `div` 10) ++ [ x `mod` 10 ]
 
split :: Char -> String -> [String]
split c s =  
    case dropWhile (==c) s of
        "" -> []
        s' -> w : split c s''
            where (w, s'') = break (==c) s'

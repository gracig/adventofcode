import qualified Control.Monad.Writer as Writer

test::Integer -> Writer.Writer [String] Integer
test x = do
    Writer.tell ["Number = " ++ (show x)]
    return (x+x)
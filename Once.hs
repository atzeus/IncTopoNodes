module Once where

import System.IO.Unsafe

{-| @once m@ gives an IO action such that the first time it is run it just does @m@, and any time afterwards it returns the result of the first run (i.e. it only runs @m@ once).
-}
{-# NOINLINE once #-}
once :: IO a -> IO a
once m = return x >>= \y -> y `seq` return y
     where x = unsafePerformIO m

main :: IO ()
main = once (putStrLn "Bla") `seq` return ()

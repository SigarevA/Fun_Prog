module Main where

import Lib
import Control.Monad.Random
import Data.List

main :: IO ()
-- main = putStrLn $ show $ calLosses [True, True, False, True, True]
--main =  putStrLn $ show $ attack [1 .. 5] [5, 4 .. 1]
main = putStrLn $ show $ evalRand ( successProb $ Battlefield 10 10) (mkStdGen 98)
--main = do 
--    val <- evalRandIO ( successProb $ Battlefield 10 10)
    -- putStrLn $ show val
-- main = do 
--    val <- evalRandIO ( battle $ Battlefield 10 10)
--    putStrLn $ show val
-- main = putStrLn $ show $ ( DV 5) > (DV 5)
-- main = do 
--    val <- evalRandIO ( generateDie 13)
--    putStrLn $ show val

--main = putStrLn "show $ length ( generateDieAtt 3) "
-- main = do
--     val <- head (generateDie 5)
--     print val

module Main where

import Lib
import Control.Monad.Random
import Data.List

main :: IO ()
main = do 
    val <- evalRandIO ( successProb $ Battlefield 4 4)
    print val
-- main = do 
--    val <- evalRandIO ( battle $ Battlefield 10 10)
--    print val
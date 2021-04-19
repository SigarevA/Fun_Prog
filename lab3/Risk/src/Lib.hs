module Lib
    ( someFunc
    ) where



newtype DieValue = DV { 
    unDV :: Int 
}
type Army = Int


someFunc :: IO ()
someFunc = putStrLn "sad"

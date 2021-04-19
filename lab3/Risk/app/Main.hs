module Main where

import Lib
import Control.Monad.Random
import Data.List


newtype DieValue = DV { 
    unDV :: Int 
} deriving (Show, Eq, Ord)

data Battlefield = Battlefield {
    attackers :: Army,
    defenders :: Army
} deriving(Show)
 
-- first f (a, c) = (f a, c)
convert (a, c) = ( DV a, c) 

instance Random DieValue where 
    random = convert . randomR (1, 6)

type Army = Int

main :: IO ()
getLen :: [a] -> Double
getLen values = helper values 0
    where 
        helper [] acc = acc 
        helper (x : xs) acc = helper xs (acc + 1) 

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


-- generateDie n = replicate n ((\ x -> evalRandIO testv) 0)
generateDie:: Int -> Rand StdGen [DieValue]
generateDie n =  reverse . sort <$> replicateM n testv
generateDieAtt n 
   | n > 3 = generateDie 3
   | otherwise = generateDie (n - 1)

generateDieDef n 
   | n > 2 = generateDie 2
   | otherwise = generateDie n

attack :: [DieValue] -> [DieValue] -> [Bool]
attack = zipWith (>)

calLosses :: [Bool] -> (Int, Int)
calLosses results = ( helper, length results - helper)
    where 
    helper = length (filter id results)

battle :: Battlefield -> Rand StdGen Battlefield
battle battlefield = do
    attacker <- generateDieAtt attackerCount
    defender <- generateDieDef defenderCount
    let losses = calLosses $ attack attacker defender
    return (Battlefield (attackerCount - fst losses) ( defenderCount  - snd losses ))
    where 
        attackerCount = attackers battlefield
        defenderCount = defenders battlefield

invade :: Battlefield -> Rand StdGen Battlefield
invade battlefield 
    | attackers battlefield == 1 || defenders battlefield == 0 = return battlefield
    | otherwise = do 
        newBf <- battle battlefield
        invade newBf 

successProb :: Battlefield -> Rand StdGen Double
successProb battlefield = (/ 1000) . getLen . filter id . map (\bf -> attackers bf == 1) <$> replicateM 1000 ( invade battlefield)
-- invade battlefield = (/ 1000) ( length . filter id . map (\bf -> attackers bf == 1) ) <$> replicateM 1000 battlefield 

-- test =  do
--    stdGen <- getStdGen
--    let r = evalRand testv stdGen :: Int
--    putStrLn $ "Result: " ++ show r 
testv :: Rand StdGen DieValue
testv = getRandom   
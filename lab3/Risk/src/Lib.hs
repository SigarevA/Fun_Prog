module Lib
    ( battle 
    , invade
    , successProb
    , Battlefield (..)
    , DieValue ( .. )
    ) where

import Control.Monad.Random
import Data.List

newtype DieValue = DV { 
    unDV :: Int 
} deriving (Show, Eq, Ord)

data Battlefield = Battlefield {
    attackers :: Army,
    defenders :: Army
} deriving(Show, Eq, Ord)
 
convert (a, c) = ( DV a, c) 

instance Random DieValue where 
    random = convert . randomR (1, 6)

type Army = Int 

getLen :: [a] -> Double
getLen values = helper values 0
    where 
        helper [] acc = acc 
        helper (x : xs) acc = helper xs (acc + 1) 

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
successProb battlefield = (/ 1000) . getLen . filter id . map (\bf -> defenders bf == 0) <$> replicateM 1000 ( invade battlefield)

testv :: Rand StdGen DieValue
testv = getRandom
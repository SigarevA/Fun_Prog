
module Main(main) where 

import Test.Hspec
import Lib
import Control.Monad.Random

main :: IO ()
main = hspec $ describe "Testing Risk Game" $ do
    
    -- battle 
    describe "Attacker lost two squads" $
        it "bf 10 10 -> bf 8 10" $ 
            evalRand (battle  (Battlefield 10 10) ) (mkStdGen 98) `shouldBe` Battlefield 8 10
    
    -- battle 
    describe "Equal losses" $
        it "bf 10 10 -> bf 9 9" $ 
            evalRand (battle  (Battlefield 10 10) ) (mkStdGen 69) `shouldBe` Battlefield 9 9
    
    -- battle 
    describe "Attacker retained two squads" $
        it "bf 10 10 -> bf 10 8" $ 
            evalRand (battle  (Battlefield 10 10) ) (mkStdGen 67) `shouldBe` Battlefield 10 8


    -- battle 
    describe "Attacker has only one squad" $
        it "No attack" $ 
            evalRand (battle  (Battlefield 1 3) ) (mkStdGen 67) `shouldBe` Battlefield 1 3

    -- battle 
    describe "Defender has no squad" $
        it "No attack" $ 
            evalRand (battle  (Battlefield 5 0) ) (mkStdGen 67) `shouldBe` Battlefield 5 0        

    -- invade
    describe "Defender defeated" $
        it "bf 150 150 -> bf 2 0" $
            evalRand (invade (Battlefield 150 150)) (mkStdGen 421) `shouldBe` Battlefield 2 0  
    
    -- invade 
    describe "Attacker defeated" $
        it "bf 150 150 -> bf 1 11" $
            evalRand (invade (Battlefield 150 150)) (mkStdGen 75) `shouldBe` Battlefield 1 11

        -- invade 
    describe "Attacker defeated" $
        it "bf 20 20 -> bf 1 3" $
            evalRand (invade (Battlefield 20 20)) (mkStdGen 79) `shouldBe` Battlefield 1 3

    -- successProb
    describe "The probability of the attacker victory" $
        it "bf 2 1 -> 0.563" $
            evalRand (successProb (Battlefield 2 1)) (mkStdGen 2475) `shouldBe` 0.563
        
    -- successProb
    describe "The probability of the attacker victory" $
        it "bf 5 5 -> 0.475" $
            evalRand (successProb (Battlefield 5 5)) (mkStdGen 902231) `shouldBe` 0.475

    -- successProb
    describe "The probability of the attacker victory" $
        it "bf 5 5 -> 0.464" $
            evalRand (successProb (Battlefield 5 5)) (mkStdGen 143779) `shouldBe` 0.464

    -- successProb
    describe "The probability of the attacker victory" $
        it "bf 50 50 -> 0.257" $
            evalRand (successProb (Battlefield 50 50)) (mkStdGen 143779) `shouldBe` 0.257
        
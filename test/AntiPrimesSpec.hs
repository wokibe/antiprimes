module AntiPrimesSpec where

import qualified AntiPrimes as AP
import Control.Exception
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "size" $ do
    it "should deliver the expected result for 1" $ do
      (AP.size 1) `shouldBe` 1
    it "should deliver the expected result for 2" $ do
      (AP.size 2) `shouldBe` 2
    it "should deliver the expected result for 60" $ do
      (AP.size 60) `shouldBe` 12
    it "should deliver the expected result for 5040" $ do
      (AP.size 5040) `shouldBe` 60
    it "should throw an exception for 0" $ do
      evaluate (AP.size 0) `shouldThrow` errorCall
                                          "the number must be greater than 0"
    it "should throw an exception for -1" $ do
      evaluate (AP.size (-1)) `shouldThrow` errorCall
                                          "the number must be greater than 0"
  describe "dividers" $ do
    it "should deliver the expected result for 1" $ do
      (AP.dividers 1) `shouldBe` [1]
    it "should deliver the expected result for 2" $ do
      (AP.dividers 2) `shouldBe` [1,2]
    it "should deliver the expected result for 60" $ do
      (AP.dividers 60) `shouldBe` [1,2,3,4,5,6,10,12,15,20,30,60]
    it "should deliver the expected result for 5040" $ do
      (AP.dividers 5040) `shouldBe`
                [1,2,3,4,5,6,7,8,9,10,12,14,15,16,18,20,
                21,24,28,30,35,36,40,42,45,48,56,60,63,
                70,72,80,84,90,105,112,120,126,140,144,168,
                180,210,240,252,280,315,336,360,420,504,
                560,630,720,840,1008,1260,1680,2520,5040]
    it "should throw an exception for 0" $ do
      evaluate (AP.dividers 0) `shouldThrow` errorCall
                                          "the number must be greater than 0"
    it "should throw an exception for -1" $ do
      evaluate (AP.dividers (-1)) `shouldThrow` errorCall
                                          "the number must be greater than 0"
  describe "proof" $ do
    it "should deliver the expected result for 1" $ do
      (AP.proof 1) `shouldBe` True
    it "should deliver the expected result for 2" $ do
      (AP.proof 2) `shouldBe` True
    it "should deliver the expected result for 3" $ do
      (AP.proof 3) `shouldBe` False
    it "should deliver the expected result for 5" $ do
      (AP.proof 5) `shouldBe` False
    it "should deliver the expected result for 60" $ do
      (AP.proof 60) `shouldBe` True
    it "should deliver the expected result for 5040" $ do
      (AP.proof 5040) `shouldBe` True
    it "should throw an exception for 0" $ do
      evaluate (AP.proof 0) `shouldThrow` errorCall
                                          "the number must be greater than 0"
    it "should throw an exception for -1" $ do
      evaluate (AP.proof (-1)) `shouldThrow` errorCall
                                          "the number must be greater than 0"
  describe "list" $ do
    it "should deliver the expected result for 1" $ do
      (AP.list 1) `shouldBe` [(1,1)]
    it "should deliver the expected result for 2" $ do
      (AP.list 2) `shouldBe` [(1,1),(2,2)]
    it "should deliver the expected result for 60" $ do
      (AP.list 60) `shouldBe`
                [(1,1),(2,2),(4,3),(6,4),(12,6),(24,8),(36,9),(48,10),(60,12)]
    it "should deliver the expected result for 5040" $ do
      (AP.list 5040) `shouldBe`
                  [(1,1),(2,2),(4,3),(6,4),(12,6),(24,8),
                  (36,9),(48,10),(60,12),(120,16),(180,18),
                  (240,20),(360,24),(720,30),(840,32),(1260,36),
                  (1680,40),(2520,48),(5040,60)]
    it "should throw an exception for 0" $ do
      evaluate (AP.list 0) `shouldThrow` errorCall
                                          "the number must be greater than 0"
    it "should throw an exception for -1" $ do
      evaluate (AP.list (-1)) `shouldThrow` errorCall
                                          "the number must be greater than 0"

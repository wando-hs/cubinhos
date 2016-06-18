module FaceSpec where

import Test.Hspec

import Face
import Side

import Stubs

faceText :: String
faceText = "True  False True \n\
            \False False False\n\
            \True  False False\n"

face :: Face
face = Face (A [True, False, True])
            (B [True, False, False])
            (C [False, False, True])
            (D [True, False, True])

spec :: Spec
spec =
  describe "Face" $ do
    it "should get the horizontal length" $ do
        hlenght face `shouldBe` 3
    it "should get the vertical length" $ do
        vlenght face `shouldBe` 3
    it "read must be the opposite of show" $ do
        (read . show) face `shouldBe` face
    it "should print a face" $ do
        show face `shouldBe` faceText
    it "should parse a face" $ do
        read faceText `shouldBe` face
    it "PIROCAww" $ do
        shouldBe (fullSide face3 d3) (True,d3,False)
    it "PIROCA" $ do
        shouldBe (Face.match (True,d3,False) (True,a4,True)) False
    it "should match complementary sides between faces" $ do
        matches face3 face4 `shouldMatchList` [(a3, b4), (a3, d4),
                                               (b3, b4),
                                               (c3, a4),
                                               (d3, c4)]

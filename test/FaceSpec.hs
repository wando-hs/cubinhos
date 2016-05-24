module FaceSpec where

import Test.Hspec

import Face
import Side

face1Text :: String
face1Text = "True  False True \n\
            \False False False\n\
            \True  False False\n"

face2Text :: String
face2Text = "True  True  False\n\
            \False False True \n\
            \True  False True \n"

face1 :: Face
face1 = Face (A [True, False, True])
             (B [True, False, False])
             (C [False, False, True])
             (D [True, False, True])

face2 :: Face
face2 = read face2Text

side :: Side
side = A [False, True, False]

spec :: Spec
spec =
  describe "Face" $ do
    it "should get the horizontal length" $ do
        hlenght face1 `shouldBe` 3
    it "should get the vertical length" $ do
        vlenght face1 `shouldBe` 3
    it "read must be the opposite of show" $ do
        (read . show) face1 `shouldBe` face1
    it "should print a face" $ do
        show face1 `shouldBe` face1Text
    it "should parse a face" $ do
        read face1Text `shouldBe` face1
    it "should match complementary sides" $ do
        matchSide face1 side  `shouldBe` [(A [True, False, True]), (D [True, False, True])]
    it "should match complementary sides between faces" $ do
        matchFace face1 face2 `shouldBe` [(C [False, False, True ], A [True,  True, False]),
                                          (B [True,  False, False], B [False, True, True])]

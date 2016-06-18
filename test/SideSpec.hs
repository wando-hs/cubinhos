module SideSpec where

import Test.Hspec

import  Side

side1 :: Side
side1 = A [True, True, False, True, False]

side2 :: Side
side2 = A [True, False, True, False, False]

side3 :: Side
side3 = A [True, True, False, False, False]

spec :: Spec
spec =
  describe "Side" $ do
    it "should match complementary sides" $ do
        match side1 side2  `shouldBe` True
    it "should not match uncomplementary sides" $ do
        match side1 side3  `shouldBe` False

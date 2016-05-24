module SideSpec where

import Test.Hspec

import  Side

side1 :: Side
side1 = A [True, False, True]

side2 :: Side
side2 = A [False, True, False]

side3 :: Side
side3 = A [True, False, False]

spec :: Spec
spec =
  describe "Side" $ do
    it "should match complementary sides" $ do
        match side1 side2  `shouldBe` True
    it "should not match uncomplementary sides" $ do
        match side1 side3  `shouldBe` False

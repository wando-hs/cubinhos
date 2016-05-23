module FaceSpec where

import Test.Hspec

import Face
import Side

-- True  False True
-- False       False
-- True  False False
face1 = Face (A [True, False, True])
             (B [True, False, False])
             (C [False, False, True])
             (D [True, False, True])

-- True  True  False
-- False       False
-- True  False True
face2 = Face (A [True, True, False])
             (B [False, True, True])
             (C [True, False, True])
             (D [True, False, True])

side = A [False, True, False]

spec :: Spec
spec =
  describe "Face" $ do
    it "should match complementary sides" $ do
        matchSide face1 side  `shouldBe` [(A [True, False, True]), (D [True, False, True])]
    it "should match complementary sides between faces" $ do
        matchFace face1 face2 `shouldBe` [(C [False, False, True ], A [True,  True, False]),
                                          (B [True,  False, False], B [False, True, True])]

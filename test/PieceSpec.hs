module PieceSpec where

import Test.Hspec

import Piece
import Side(Side(..))
import Face(Face(..))

import Stubs

ba6 = A [False, False, True, False, False]
bb6 = B [False, False, True, False, False]
bc6 = C [False, True, False, True, False]
bd6 = D [False, True, False, True, False]
backFace6 = Face ba6 bb6 bc6 bd6


spec :: Spec
spec =
  describe "Face" $ do
    it "turn" $ do
        turn piece6 `shouldBe` Piece Back 6 backFace6
    it "bobrinha" $ do
        match piece4 piece6 `shouldMatchList` [((Front, b4), (Front, a6)),
                                             ((Front, a4), (Front, b6)),
                                             ((Front, c4), (Front, b6)),
                                             ((Front, c4), (Front, c6)),
                                             ((Front, b4), (Front, d6)),
                                             ((Front, d4), (Front, d6)),
                                             ((Front, d4), (Front, a6)),
                                             ((Front, a4), (Back, bc6)),
                                             ((Front, b4), (Back, ba6)),
                                             ((Front, b4), (Back, bb6)),
                                             ((Front, c4), (Back, bc6)),
                                             ((Front, c4), (Back, bd6)),
                                             ((Front, d4), (Back, ba6)),
                                             ((Front, d4), (Back, bb6))]


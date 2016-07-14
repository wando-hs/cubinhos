module Piece(FaceName(..), match, Piece(..), turn) where

import Face (Face(..))
import Side (Side(..), SideName(..))

import qualified Side as S
import qualified Face as F

data FaceName = Front | Back
    deriving (Eq, Show)

data Piece = Piece FaceName Int Face
    deriving (Eq, Show)

otherFace :: Face -> Face
otherFace (Face a b c d) = Face (A $ rev a)
                                (B $ rev d)
                                (C $ rev c)
                                (D $ rev b)
    where rev = reverse . S.raw

turn :: Piece -> Piece
turn (Piece Front number face) = Piece Back number (otherFace face)
turn (Piece Back number face) = Piece Front number (otherFace face)

getFace :: Piece -> Face
getFace (Piece Front _ face) = face
getFace (Piece Back _ face) = face

match :: Piece -> Piece -> [( (FaceName, Side), (FaceName,Side) )]
match (Piece _ _ face1) (Piece _ _ face2) = concatMap filtrae faces
    where faces = [(Front, face2), (Back, otherFace face2)]
          filtrae (name, face) = map (\ (s1, s2) -> ( (Front, s1), (name, s2) )) $ F.matches face1 face

data SideName = A | B | C | D
newtype Coordinate = Coordinate Int Facename SideName
type Connection = (Coordinate, Coordinate)

-- [P1, P2, P3, P4, P5, P6]
-- (Front 4 A, Back 6 C)
-- (Front 2 A, Back 4 B)
-- (Front 2 D, Back 6 A)

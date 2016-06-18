module Piece(Piece(..), turn) where

import Face (Face(..))
import Side (Side(..))

import qualified Side as S

data Piece = Front Int Face | Back Int Face

otherFace :: Face -> Face
otherFace (Face a b c d) = Face (A $ rev a)
                                (B $ rev d)
                                (C $ rev c)
                                (D $ rev b)
    where rev = reverse . S.raw

turn :: Piece -> Piece
turn (Front number face) = Back number $ otherFace face
turn (Back number face) = Front number $ otherFace face

getFace :: Piece -> Face
getFace (Front _ face) = face
getFace (Back _ face) = face

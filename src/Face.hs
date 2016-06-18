module Face (Face (..), matches,match,fullSide, hlenght, vlenght) where

import qualified Side as S
import Side (Side(..))

data Face = Face Side Side Side Side
    deriving (Eq)

instance Show Face where
    show = unlines . map (unwords . map stringify) . toArray
        where stringify x = if x == True then "True " else "False"

instance Read Face where
    readsPrec _ r = [(readData r, "")]
        where readData = fromArray . map (map read . words) . lines

fromArray :: [[Bool]] -> Face
fromArray face = Face (A $ head face)
                      (B $ map last face)
                      (C $ reverse $ last face)
                      (D $ reverse $ map head face)

toArray :: Face -> [[Bool]]
toArray face@(Face a b c d) = [S.raw a] ++ from center ++ [reverse $ S.raw c]
    where center = tail $ init $ zip (reverse $ S.raw d) (S.raw b)
          from = map (\(f,l) -> [f] ++ (replicate size False) ++ [l])
          size = vlenght face - 2

hlenght :: Face -> Int
hlenght (Face a _ _ _) = length $ S.raw a

vlenght :: Face -> Int
vlenght (Face _ b _ _) = length $ S.raw b

sides :: Face -> [Side]
sides (Face a b c d) = [a, b, c, d]

type FullSide = (Bool, Side, Bool)

side :: FullSide -> Side
side (_, s, _) = s

topPenultimate :: Side -> Bool
topPenultimate = last . init . S.raw

bottomPenultimate :: Side -> Bool
bottomPenultimate = head . tail . S.raw

matchBorder :: Bool -> Bool -> Bool -> Bool -> Bool
matchBorder p q r s = (not q) && r || not (r || (s && p && not q))

match :: FullSide -> FullSide -> Bool
match (a, side, c) (a2, side2, c2) = middle && top && bottom
    where top = matchBorder a (head $ S.raw side) (last $ S.raw side2) c2
          middle = S.match side side2
          bottom = matchBorder c (last $ S.raw side) (head $ S.raw side2) a2

fullSide :: Face -> Side -> FullSide
fullSide (Face a b _ d) (A _) = (topPenultimate d, a, bottomPenultimate b)
fullSide (Face a b c _) (B _) = (topPenultimate a, b, bottomPenultimate c)
fullSide (Face _ b c d) (C _) = (topPenultimate b, c, bottomPenultimate d)
fullSide (Face a _ c d) (D _) = (topPenultimate c, d, bottomPenultimate a)

matchSide :: Face -> FullSide -> [Side]
matchSide face fside = map side $ filter (match fside) $ map (fullSide face) $ sides face

matches :: Face -> Face -> [(Side, Side)]
matches f1 f2 = concatMap joinMatches $ sides f2
    where joinMatches side = map (\ s -> (s, side)) $  matchSide f1 $ fullSide f2 side

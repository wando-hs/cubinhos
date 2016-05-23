module Face (Face (..), matchSide, matchFace, toArray, fromArray) where

import qualified Side as S

data Face = Face S.Side S.Side S.Side S.Side
    deriving (Eq)

instance Show Face where
    show = unlines . map (unwords . map stringify) . toArray
        where stringify x = if x == True then "True " else "False"

instance Read Face where
    readsPrec _ r = [(readData r, "")]
        where readData = fromArray . map (map read . words) . lines

fromArray :: [[Bool]] -> Face
fromArray face = Face (S.A $ head face)
                      (S.B $ map last face)
                      (S.C $ reverse $ last face)
                      (S.D $ reverse $ map head face)

toArray :: Face -> [[Bool]]
toArray face@(Face a b c d) = [S.raw a] ++ from center ++ [reverse $ S.raw c]
    where center = tail $ init $ zip (reverse $ S.raw d) (S.raw b)
          from = map (\(f,l) -> [f] ++ (replicate size False) ++ [l])
          size = vlenght face - 2

hlenght :: Face -> Int
hlenght (Face a _ _ _) = length $ S.raw a

vlenght :: Face -> Int
vlenght (Face _ b _ _) = length $ S.raw b

sides :: Face -> [S.Side]
sides (Face a b c d) = [a, b, c, d]

matchSide :: Face -> S.Side -> [S.Side]
matchSide face side = filter (S.match side) $ sides face

matchFace :: Face -> Face -> [(S.Side, S.Side)]
matchFace f1 f2 = concatMap joinMatches $ sides f2
    where joinMatches side = map (\ s -> (s, side)) $  matchSide f1 side

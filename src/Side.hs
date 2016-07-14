module Side (Side (..), match, raw, SideName(..)) where

data Side = A [Bool] | B [Bool] | C [Bool] | D [Bool]
    deriving (Show, Eq)

xor :: Bool -> Bool -> Bool
xor True = not
xor False = id

raw :: Side -> [Bool]
raw (A s) = s
raw (B s) = s
raw (C s) = s
raw (D s) = s

match :: Side -> Side -> Bool
match s1 s2 = all (uncurry xor) $ zip (middle s1) (reverse $ middle s2)
    where middle = init . tail . raw

data SideName = A | B | C | D


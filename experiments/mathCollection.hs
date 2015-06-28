import Data.Ratio

cfc :: Int -> Double
cfc 0 = 1
cfc 1 = 1 + 1 /  2
cfc 2 = 1 + 1 / (2 + 1 /  3)
cfc 3 = 1 + 1 / (2 + 1 / (3 + 1 /  4))
cfc 4 = 1 + 1 / (2 + 1 / (3 + 1 / (4 + 1 / 5)))

-- cfc1 k = k + 1 / (k + 1)


cfc'' 0 = 1
cfc'' 1 = cfc' 1
cfc'' 2 = 1 + 1 / cfc' 2
cfc'' 3 = 1 + 1 / (2 + 1 / cfc' 3)
cfc'' 4 = 1 + 1 / (2 + 1 / (3 + 1 / cfc' 4))


-- cfc' :: Int -> Double
cfc' n = cfc1 0 where
    cfc1 k | k < n = k + 1 / cfc1 (k+1)
           | otherwise = k + 1
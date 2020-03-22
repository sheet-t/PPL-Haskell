import Data.List (nub, permutations)

check f = length . nub . zipWith f [0..]

generate n = filter (\x -> check (+) x == n && check (-) x == n) $ permutations [0..n-1]

main = print $ generate 8 

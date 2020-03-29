{-# LANGUAGE ParallelListComp #-}

-- CS 3210 - Principles of Programming Languages - Spring 2020
-- Programming Assignment 02 - The N-queens Problem
-- Author(s):

import Data.List
import GHC.Char

type Seq   = [Char]
type Board = [Seq]

count :: (a -> Bool) -> [a] -> Int
count pred = length . filter pred

-- TODOd 01/17
setup :: Int -> Board
setup n = [[ '-' | y <- [1..n4]] | x <- [1..n4] ]
  where n4 = max n 4

-- TODOd 02/17
rows :: Board -> Int
rows = length

-- TODOd 03/17
cols :: Board -> Int
cols = length . head

-- TODOd 04/17
size :: Board -> Int
size b = if rows b == cols b then rows b else 0

-- TODOd 05/17
queensSeq :: Seq -> Int
queensSeq = count $ eqChar 'Q'

-- TODOd 06/17
queensBoard :: Board -> Int
queensBoard = sum . map queensSeq

-- TODOd 07/17
seqValid :: Seq -> Bool
seqValid s = queensSeq s <= 1

-- TODOd 08/17
rowsValid :: Board -> Bool
rowsValid b = all seqValid b

-- TODOd 09/17
colsValid :: Board -> Bool
colsValid b = all seqValid (colWise b)
  where colWise b = [[ (b!!y)!!x | y <- [0..(size b) - 1 ]] | x <- [0..(size b) - 1 ]]

-- TODOd 10/17
diagonals :: Board -> Int
diagonals b = 2 * (size b) - 1

mainDiagIndices :: Board -> Int -> [ (Int, Int) ]
mainDiagIndices b p
  | p < n = [ (n - 1 - qr, q) | q <- [0..p] | qr <- [p,p-1..0] ]
  | otherwise = [ (q, (n - 1 - qr)) | q <- [0..2 * (n - 1) - p] | qr <- [2 * (n - 1) - p,2 * (n - 1) - p - 1..0] ]
  where n = size b

-- TODOd 11/17
allMainDiagIndices :: Board -> [[ (Int, Int) ]]
allMainDiagIndices b = [mainDiagIndices b i | i <- [0..(diagonals b) - 1]]

-- TODOd 12/17
mainDiag :: Board -> [Seq]
-- mainDiag b = [[(b !! y) !! x | (x,y) <- diag ] diag <- allMainDiagIndices b]
mainDiag b = map (\diag -> map (\(x,y) -> (b !! y) !! x ) diag ) $ allMainDiagIndices b

secDiagIndices :: Board -> Int -> [ (Int, Int) ]
secDiagIndices b p
  | p < n = [ (p - q, q) | q <- [0..p] ]
  | otherwise = [ (p - (n - 1 - q), n - 1 - q) | q <- [2 * (n - 1) - p, 2 * (n - 1) - p - 1..0] ]
  where n = size b

-- TODOd 13/17
allSecDiagIndices :: Board -> [[ (Int, Int) ]]
allSecDiagIndices b = [secDiagIndices b i | i <- [0..(diagonals b) -1 ]]

-- TODOd 14/17
secDiag :: Board -> [Seq]
-- secDiag b = [[(b !! y) !! x | (x,y) <- diag ] diag <- allSecDiagIndices b]
secDiag b = map (\diag -> map (\(x,y) -> (b !! y) !! x ) diag ) $ allSecDiagIndices b

-- TODOd 15/17
diagsValid :: Board -> Bool
diagsValid b = all seqValid (mainDiag b ++ secDiag b)

-- TODOd 16/17
valid :: Board -> Bool
valid b = rowsValid b && colsValid b && diagsValid b

-- TODOd 17/17 (¡Phew!)
solved :: Board -> Bool
solved b = valid b && size b == queensBoard b

setQueenAt :: Board -> Int -> [Board]
setQueenAt b i = do
  let z = replicate ((size b) - 1) '-'
  let p = nub (permutations ("Q" ++ z))
  [ [ (b!!k) | k <- [0..(i-1)] ] ++ [r] ++ [ (b!!k) | k <- [(i+1)..((rows b) - 1)] ] | r <- p ]

nextRow :: Board -> Int
nextRow b = head [ i | i <- [0 .. (size b) - 1], queensSeq (b!!i) == 0 ]

solve :: Board -> [Board]
solve b
  | solved b = [b]
  | otherwise = concat [ solve newB | newB <- setQueenAt b i, valid newB ]
    where i = nextRow b

main = do
  let b = setup 6
  let solution = [ solution | solution <- solve b ]
  print (solution)

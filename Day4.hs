module Day4 where

import Debug.Trace
import Data.List
import Data.List.Split

data N = N {checked :: Bool, num :: Int}
  deriving (Show,Eq)

parse :: String -> ([Int],[[[N]]])
parse i = (nums,boards)
  where (numInput:boardInput) = lines i
        nums = read ("["++numInput++"]")
        boardsInputs = splitWhen null boardInput
        boards = filter (not.null) $ map (map (map (N False . read) . words)) boardsInputs

input = fmap parse (readFile "input.4")

example = fmap parse (readFile "example.4")

check i (N b i')
  | i == i' = N True i'
  | otherwise = N b i'

mark number board = (map.map) (check number) board

completed :: [[N]] -> Bool
completed board = any (all checked) board || any (all checked) (transpose board)

score n b = n * sum (map (sum . map num . filter (not.checked)) b)

part1 (n:nums,boards) =
  case filter completed boards' of
    (b:_) -> score n b
    _ -> part1 (nums,boards')
  where boards' = map (mark n) boards

part2 (n:nums,boards) =
  case partition completed boards' of
    (w:_,[]) -> score n w
    (_,rest) -> part2 (nums,rest)
  where boards' = map (mark n) boards

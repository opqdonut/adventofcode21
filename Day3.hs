module Day3 where

import Data.Char
import Data.List
import Data.Ord
import Numeric
import Control.Applicative

input = map (map digitToInt) . lines <$> readFile "input.3"

testInput :: [[Int]]
testInput = [[0,0,1,0,0],
             [1,1,1,1,0],
             [1,0,1,1,0],
             [1,0,1,1,1],
             [1,0,1,0,1],
             [0,1,1,1,1],
             [0,0,1,1,1],
             [1,1,1,0,0],
             [1,0,0,0,0],
             [1,1,0,0,1],
             [0,0,0,1,0],
             [0,1,0,1,0]]

inv 0 = 1
inv 1 = 0

invert = map inv

freqs = map cnt . group . sort
  where cnt xs = (head xs,length xs)

common = fst . maximumBy (comparing snd)

bin :: [Int] -> Int
bin x = go 0 x
  where go a [] = a
        go a (b:bs) = go (2*a+b) bs

part1 :: [[Int]] -> Int
part1 i =
  let x = map common .
          map freqs $
          transpose i
  in bin x * bin (invert x)

which bits
  | zeros>ones = 0
  | otherwise = 1
  where [(0,zeros),(1,ones)] = freqs bits

whittle :: Bool -> [[Int]] -> [Int]
whittle _ [x] = x
whittle flip bits =
  let f = if flip then inv else id
      target = f $ which (map head bits)
      rest = [bs | (b:bs) <- bits, b == target]
  in target : whittle flip rest

part2 i =
  let o2 = whittle False i
      co2 = whittle True i
  in bin o2 * bin co2

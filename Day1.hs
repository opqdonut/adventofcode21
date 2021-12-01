module Day1 where

input :: IO [Int]
input = fmap (map read.lines) (readFile "input.1")

part1 input =
  length .
  filter (uncurry (<)) $
  zip input (tail input)

convolve :: [Int] -> [Int]
convolve (a:b:c:xs) = a+b+c:convolve (b:c:xs)
convolve _ = []

part2 = part1 . convolve

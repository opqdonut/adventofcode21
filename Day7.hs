module Day7 where

import Data.List

parse :: String -> [Int]
parse s = read ("["++s++"]")

input = parse <$> readFile "input.7"

example = [16,1,2,0,4,2,7,1,2,14]

part1 i =
  let s = sort i
      median = s !! (length s `div` 2)
  in sum [abs (x-median) | x <- s]

cost diff = d*(d+1)`div`2
  where d = abs diff

-- the average should be the best mean square error estimator of a dataset
part2 i =
  -- why is floor right and not round?
  let avg = floor (fromIntegral (sum i) / fromIntegral (length i))
  in sum [cost (x-avg) | x <- i]

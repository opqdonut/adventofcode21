module Day2 where

import Data.List

parse :: [String] -> (Int,Int)
parse ["forward",x] = (read x, 0)
parse ["down",y] = (0, read y)
parse ["up",y] = (0, negate (read y))

input :: IO [(Int,Int)]
input = fmap (map (parse.words).lines) (readFile "input.2")

example1 = map (parse.words)
  ["forward 5"
  ,"down 5"
  ,"forward 8"
  ,"up 3"
  ,"down 8"
  ,"forward 2"
  ]

f (a,b) (c,d) = (a+c,b+d)

part1 i = let (x,y) = foldl' f (0,0) i
          in x*y


g (x0,y0,aim) (x1,0) = (x0+x1,y0+x1*aim,aim)
g (x0,y0,aim) (0,aim') = (x0,y0,aim+aim')

part2 i = let (x,y,_) = foldl' g (0,0,0) i
          in x*y

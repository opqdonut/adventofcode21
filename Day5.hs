module Day5 where

import qualified Data.Map as M
import Data.List
import Data.List.Split

parse :: String -> [((Int,Int),(Int,Int))]
parse = map p . lines
  where p line = (r start, r end)
          where [start,"->",end] = splitOn " " line
                r x = read ("("++x++")")

input = fmap parse (readFile "input.5")

example = fmap parse (readFile "example.5")

updateBB ((minx,miny),(maxx,maxy)) (x,y)
  = ((min minx x, min miny y),(max maxx x, max maxy y))

updateBBLine bb (start,end) =
  updateBB (updateBB bb start) end

between a b x = min a b <= x && x <= max a b

covers ((x0,y0),(x1,y1))
  | x0==x1 || y0==y1 = [(x,y) | x <- [min x0 x1..max x0 x1], y <- [min y0 y1..max y0 y1]]
  | otherwise = []

dot m c = M.insertWith (+) c (1::Int) m

paint m l = foldl' dot m (covers l)

chart ls = foldl' paint M.empty ls

part1 ls = length [c | (c,n) <- M.assocs (chart ls), n>1]

covers2 ((x0,y0),(x1,y1)) = zip (range x0 x1) (range y0 y1)
  where range a b
          | a==b = repeat a
          | a>b = [a,a-1..b]
          | otherwise = [a..b]

paint2 m l = foldl' dot m (covers2 l)

chart2 ls = foldl' paint2 M.empty ls

part2 ls = length [c | (c,n) <- M.assocs (chart2 ls), n>1]

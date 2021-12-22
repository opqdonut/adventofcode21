module Day22 where

import Data.List
import qualified Data.Set as S
import Data.List.Split

type Coord = (Int,Int,Int)
type Range = (Int,Int)
type Rule = (Bool,Range,Range,Range)

parseLine :: String -> Rule
parseLine l = (state=="on",limx,limy,limz)
  where [state,limits] = words l
        parseRange r = let [a,b] = splitOn ".." $ drop 2 r
                       in (read a, read b)
        [limx,limy,limz] = map parseRange $ splitOn "," limits

parse = map parseLine . lines

example = parse <$> readFile "example.22"
input = parse <$> readFile "input.22"

clamp :: Int -> Int
clamp a = min (max a (-50)) 50

inRange a = a <= 50 && a >= -50

range a0 a1
  | a1 < -50 = []
  | a0 > 50 = []
  | otherwise = [max (-50) a0..min 50 a1]

apply :: S.Set Coord -> Rule -> S.Set Coord
apply s (b,(x0,x1),(y0,y1),(z0,z1)) = foldl' f s coords
  where coords = [(x,y,z) | x <- range x0 x1
                          , y <- range y0 y1
                          , z <- range z0 z1]
        f = flip $ if b then S.insert else S.delete

part1 i = S.size $ foldl' apply S.empty i

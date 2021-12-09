module Day9 where

import Data.Char
import Data.List
import qualified Data.Map as M

type Map = M.Map (Int,Int) Int

parse :: String -> Map
parse i = M.fromList [((x,y),digitToInt c) | (x,l) <- zip [0..] ls, (y,c) <- zip [0..] l]
  where ls = lines i

input = fmap parse (readFile "input.9")
example = fmap parse (readFile "example.9")

isLow :: Map -> (Int,Int) -> Bool
isLow m (x,y) = and [m M.! (x,y) < M.findWithDefault 10 c m
                    | c <- [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]]

lows m = filter (isLow m) (M.keys m)

part1 m = sum $ map (succ . (m M.!)) (lows m)

type Color = M.Map (Int,Int) (Int,Int)

flood' m c [] = c
flood' m c ((x,y):ps) = flood' m c' (valids++ps)
  where this = c M.! (x,y)
        neighs = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
        news = filter (`M.notMember` c) neighs
        valids = filter (\p -> M.findWithDefault 9 p m /= 9) news
        c' = M.union c (M.fromList [(p,this) | p <- valids])

flood m = flood' m c ls
  where ls = lows m
        c = M.fromList (zip ls ls)

basins m = map length (group (sort (M.elems (flood m))))

part2 m = product (take 3 (reverse (sort (basins m))))

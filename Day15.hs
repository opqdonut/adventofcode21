module Day15 where

-- Had to compile this day to make it run in under a minute:
--stack ghc -- --make -main-is Day15 -O2 Day15.hs

import Debug.Trace
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char
import Data.Ord
import Data.Maybe

type Graph = M.Map (Int,Int) Int

parse :: String -> Graph
parse i = M.fromList [((x,y),digitToInt d) | (y,l) <- zip [0..] (lines i), (x,d) <- zip [0..] l]

input = parse <$> readFile "input.15"
example = parse <$> readFile "example.15"

neighbours (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

dijkstra :: Graph -> (Int,Int) -> Graph -> S.Set (Int,Int) -> Int
dijkstra g end distances wavefront
  | M.member end distances = distances M.! end
  | otherwise = dijkstra g end distances' wavefront'
  where target = minimumBy (comparing (distances M.!)) (S.elems wavefront)
        targetD = distances M.! target
        nexts = [(c,newD)
                | c <- neighbours target
                , delta <- maybeToList $ M.lookup c g
                , let newD = targetD + delta
                , newD < M.findWithDefault 99999 c distances]
        distances' = M.union (M.fromList nexts) distances
        wavefront' = S.delete target wavefront `S.union` (S.fromList [c | (c,_) <- nexts])

part1 g = dijkstra g end (M.singleton (0,0) 0) (S.singleton (0,0))
  where (end,_) = M.findMax g

embiggen g = M.unions [transp i j g | i <- [0..4], j <- [0..4]]
  where transp i j g = M.fromList [((x+w*i,y+h*j),mod (d+i+j-1) 9 +1) | ((x,y),d) <- M.assocs g]
        ((maxx,maxy),_) = M.findMax g
        w = maxx+1
        h = maxy+1

part2 g = dijkstra big end (M.singleton (0,0) 0) (S.singleton (0,0))
  where big = embiggen g
        (end,_) = M.findMax big

main = do
  i <- input
  print $ part2 i

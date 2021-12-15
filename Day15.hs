module Day15 where

-- Had to compile this day to make it run in under a minute:
--stack ghc -- --make -main-is Day15 -O2 Day15.hs

import Debug.Trace
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char
import Data.Ord

type Graph = M.Map (Int,Int) Int

parse :: String -> Graph
parse i = M.fromList [((x,y),digitToInt d) | (y,l) <- zip [0..] (lines i), (x,d) <- zip [0..] l]

input = parse <$> readFile "input.15"
example = parse <$> readFile "example.15"

neighbours (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

findPath :: Graph -> (Int,Int) -> Graph -> S.Set (Int,Int) -> Graph
findPath g end distances wavefront
  | M.member end distances = distances
  | otherwise = {-trace ("w "++show wavefront) $-} findPath g end distances' wavefront'
  where target = minimumBy (comparing (distances M.!)) (S.elems wavefront)
        targetD = distances M.! target
        nexts = filter (`M.member`g) $ neighbours target
        shouldAdd c = (targetD + g M.! c) < M.findWithDefault 99999 c distances
        nexts' = filter shouldAdd nexts
        distances' = M.union (M.fromList [(c,targetD + g M.! c) | c <- nexts']) distances
        wavefront' = S.delete target wavefront `S.union` (S.fromList nexts')

part1 g = final
  where dists = findPath g end (M.singleton (0,0) 0) (S.singleton (0,0))
        (end,_) = M.findMax g
        (_,final) = M.findMax dists

embiggen g = M.unions [transp i j g | i <- [0..4], j <- [0..4]]
  where transp i j g = M.fromList [((x+w*i,y+h*j),mod (d+i+j-1) 9 +1) | ((x,y),d) <- M.assocs g]
        ((maxx,maxy),_) = M.findMax g
        w = maxx+1
        h = maxy+1

part2 g = final
  where big = embiggen g
        (end,_) = M.findMax big
        dists = findPath big end (M.singleton (0,0) 0) (S.singleton (0,0))
        (_,final) = M.findMax dists

main = do
  i <- input
  print $ part2 i

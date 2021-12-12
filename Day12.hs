module Day12 where

import Data.Char
import Data.Ord
import Data.Function
import Data.List
import qualified Data.Map as M

parse :: String -> [(String,String)]
parse = map p . lines
  where p s = case break (=='-') s of (a,'-':b) -> (a,b)

graph ps = M.fromList [(fst (head es),map snd es) | es <- groupBy ((==)`on`fst) (sort allEdges)]
  where allEdges = ps ++ map (\(x,y) -> (y,x)) ps

input = graph . parse <$> readFile "input.12"
example1 = graph . parse <$> readFile "example1.12"
example2 = graph . parse <$> readFile "example2.12"

isSmall (c:_) = isLower c

buildPaths g end (v:vs)
  | end==v = [(v:vs)]
  | otherwise = concatMap (\v' -> buildPaths g end (v':v:vs)) nexts
  where nexts = g M.! v \\ filter isSmall vs

part1 g = length $ buildPaths g "end" ["start"]

prune old new
  | twice = new \\ smalls
  | otherwise = new \\ ["start"]
  where smalls = filter isSmall old
        twice = nub smalls /= smalls

buildPaths2 g end (v:vs)
  | end==v = [(v:vs)]
  | otherwise = concatMap (\v' -> buildPaths2 g end (v':v:vs)) nexts
  where nexts = prune (v:vs) (g M.! v)

part2 g = length $ buildPaths2 g "end" ["start"]

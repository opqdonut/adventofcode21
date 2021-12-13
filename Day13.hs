module Day13 where

import Data.List
import qualified Data.Set as S

parse :: String -> (S.Set (Int,Int),[Either Int Int])
parse i = (S.fromList $ map parseCoord coords, map parseInstr instrs)
  where (coords,_:instrs) = break (=="") (lines i)
        parseCoord s = read ("("++s++")")
        parseInstr s = case break (=='=') s of
          ("fold along x", _:x) -> Left (read x)
          ("fold along y", _:y) -> Right (read y)
          a -> error (show a)

input = parse <$> readFile "input.13"

fold paper (Left x0) = S.union left flipped
  where (left,right) = S.partition (\(x,y) -> x<x0) paper
        flipped = S.map (\(x,y) -> (x0-(x-x0),y)) right
fold paper (Right y0) = S.union top flipped
  where (top,bot) = S.partition (\(x,y) -> y<y0) paper
        flipped = S.map (\(x,y) -> (x,y0-(y-y0))) bot

part1 (paper,i:_) = S.size (fold paper i)

visualize p = putStr $ unlines [[if S.member (x,y) p then '#' else '.' | x <- [x0..x1]] | y <- [y0..y1]]
  where range as = (S.findMin as, S.findMax as)
        (x0,x1) = range $ S.map fst p
        (y0,y1) = range $ S.map snd p

part2 (paper,is) = visualize (foldl' fold paper is)

module Day20 where

import Data.List
import qualified Data.Map as M

type Rules = M.Map Int Bool
type Chart = M.Map (Int,Int) Bool

parse :: String -> (Rules, M.Map (Int,Int) Bool)
parse i = (rules,chart)
  where (r:_:rest) = lines i
        rules = M.fromList [(i,c=='#') | (i,c) <- zip [0..] r]
        chart = M.fromList [((x,y),c=='#') | (y,l) <- zip [0..] rest, (x,c) <- zip [0..] l]

input = parse <$> readFile "input.20"
example = parse <$> readFile "example.20"

bitsToInt = foldl' (\v b -> 2 * v + if b then 1 else 0) 0

nextVal rules chart bg (x,y) = rules M.! bitsToInt bits
  where bits = map (\c -> M.findWithDefault bg c chart) [(x-1,y-1),(x,y-1),(x+1,y-1)
                                                        ,(x-1,y),(x,y),(x+1,y)
                                                        ,(x-1,y+1),(x,y+1),(x+1,y+1)]

nextChart :: Rules -> Chart -> Bool -> Chart
nextChart rules chart bg = M.fromList [((x,y), nextVal rules chart bg (x,y))
                                      | x <- [xmin..xmax], y <- [ymin..ymax]]
  where ((x0,y0),_) = M.findMin chart
        ((x1,y1),_) = M.findMax chart
        xmin = x0 - 1
        xmax = x1 + 1
        ymin = y0 - 1
        ymax = y1 + 1

nextBg rules bg = rules M.! bitsToInt (replicate 9 bg)

run rules chart n = go chart False n
  where go chart _ 0 = chart
        go chart bg n = go (nextChart rules chart bg) (nextBg rules bg) (n-1)

part1 (rules,chart) = length . filter id $ M.elems final
  where final = run rules chart 2

part2 (rules,chart) = length . filter id $ M.elems final
  where final = run rules chart 50

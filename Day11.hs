module Day11 where

import Data.List
import Data.Char
import qualified Data.Map as M

type Chart = M.Map (Int,Int) Int

parse :: String -> Chart
parse i = M.fromList [((x,y),digitToInt d) | (y,l) <- zip [0..] (lines i), (x,d) <- zip [0..] l]

unparse m = unlines [[intToDigit (m M.! (x,y)) | x <- [0..9]] | y <- [0..9]]

neighbours (x,y) = [(x-1,y-1),(x-1,y),(x-1,y+1)
                   ,(x,y-1),(x,y),(x,y+1)
                   ,(x+1,y-1),(x+1,y),(x+1,y+1)]

flashes :: Chart -> [(Int,Int)]
flashes m = [(x,y) | ((x,y),d) <- M.assocs m, d>9]

constant keys v = M.fromList (zip keys (repeat v))

reset coords m = M.union (constant coords 0) m

inc :: [(Int,Int)] -> Chart -> Chart
inc coords m = foldl' (\m c -> M.adjust succ c m) m coords

incNeighbours coords = inc (concatMap neighbours coords)

runFlashes :: Chart -> (Int,Chart)
runFlashes m = go m []
  where go :: Chart -> [(Int,Int)] -> (Int,Chart)
        go m old = case flashes m \\ old of
                     [] -> (length old, reset old m)
                     new -> go (incNeighbours new m) (new++old)

incAll :: Chart -> Chart
incAll = M.map succ

step m = runFlashes (incAll m)

run :: Chart -> [Int]
run m = unfoldr (Just . step) m

input = parse <$> readFile "input.11"
example = parse <$> readFile "example.11"

part1 = sum . take 100 . run

part2 m = head [i | (i,100) <- zip [1..] (run m)]

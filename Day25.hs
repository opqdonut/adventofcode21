module Day25 where

import qualified Data.Map as M

data Cucumber = East | South
  deriving (Show,Eq,Ord)

type Chart = M.Map (Int,Int) Cucumber

parse :: String -> Chart
parse i = M.fromList [((x,y),cucumber c) | (y,l) <- zip [0..] (lines i), (x,c) <- zip [0..] l, c/='.']
  where cucumber '>' = East
        cucumber 'v' = South

unparse :: Chart -> String
unparse m = unlines [[cucumber (M.lookup (x,y) m) | x <- [0..maxX]] | y <- [0..maxY]]
  where ((maxX,maxY),_) = M.findMax m
        cucumber (Just East) = '>'
        cucumber (Just South) = 'v'
        cucumber Nothing = '.'

input = parse <$> readFile "input.25"
example = parse <$> readFile "example.25"

move :: Cucumber -> (Int,Int) -> Chart -> (Int,Int) -> (Int,Int)
move East (maxX,maxY) m (x,y)
  | M.member (x',y) m = (x,y)
  | otherwise = (x',y)
  where x' = if x+1>maxX then 0 else x+1
move South (maxX,maxY) m (x,y)
  | M.member (x,y') m = (x,y)
  | otherwise = (x,y')
  where y' = if y+1>maxY then 0 else y+1

moveAll :: Cucumber -> (Int,Int) -> Chart -> Chart
moveAll target (maxX,maxY) m =
  M.fromListWithKey check [(c',dir) | (c,dir) <- M.assocs m
                                    , let c' = if dir==target
                                               then move dir (maxX,maxY) m c
                                               else c]
  where check c _ _ = error $ "Conflict at " ++ show c

step :: (Int,Int) -> Chart -> Chart
step bounds = moveAll South bounds . moveAll East bounds

bounds m = (maxX,maxY)
  where maxX = maximum $ map fst $ M.keys m
        maxY = maximum $ map snd $ M.keys m

part1 i = go 1 i
  where b = bounds i
        go n m = let m' = step b m
                 in if m'==m then n else go (n+1) m'

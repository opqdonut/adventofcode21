{-# LANGUAGE BangPatterns #-}

module Day21 where

import Debug.Trace
import Data.List
import qualified Data.Map as M

p1, p2 :: Int
p1 = 5
p2 = 10

roll die = if d>100 then d-100 else d
  where d = die+1

roll3 die = (r1+r2+r3, r3)
  where r1 = roll die
        r2 = roll r1
        r3 = roll r2

part1 p1 p2 = go p1 0 p2 0 0 0
  where go p1 p1score p2 p2score n die = let (p1',d) = move p1 die
                                             p1score' = p1' + p1score
                                         in if p1score' >= 1000
                                            then p2score * (n+3)
                                            else go p2 p2score p1' p1score' (n+3) d
        move p die = let (n,d) = roll3 die
                         p' = (mod (p+n-1) 10)+1
                     in (p',d)

-- memoize the frequencies of sums of three dice throws
freqs = map (\xs -> (head xs, length xs)) . group . sort $ [a+b+c | a <- [1..3], b <- [1..3], c <- [1..3]]

move p num = (mod (p+num-1) 10)+1

data Player = P1 | P2
  deriving (Show,Eq,Ord)
data GameState = Game Player Int Int Int Int | Won Player
  deriving (Show,Eq,Ord)

scale x m = fmap (x*) m

check (Game _ _ p1score _ p2score)
  | p1score >= 21 = Won P1
  | p2score >= 21 = Won P2
check g = g

step (Game P1 p1 p1score p2 p2score) =
  M.fromListWith (+) [(check $ Game P2 p1' (p1score+p1') p2 p2score,n)
                     |(d,n) <- freqs
                     ,let p1' = move p1 d]
step (Game P2 p1 p1score p2 p2score) =
  M.fromListWith (+) [(check $ Game P1 p1 p1score p2' (p2score+p2'),n)
                     |(d,n) <- freqs
                     ,let p2' = move p2 d]
step x = M.singleton x 1

done (Won _) = True
done _ = False

play p1 p2 = go (M.singleton (Game P1 p1 0 p2 0) 1)
  where go states
          | all done (M.keys states) = states
          | otherwise = go (M.unionsWith (+) [scale n (step s)
                                             |(s,n) <- M.assocs states])

part2 p1 p2 = maximum $ M.elems $ play p1 p2

module Day6 where

import Debug.Trace
import Data.Array

step = concatMap u
  where u 0 = [6,8]
        u n = [n-1]

fishnaive init d = length (iterate step [init] !! d)

fishrec init d = 1 + sum [fishrec i d | i <- childInits]
  where childBirths = takeWhile (<=d) [init + 1 + 7*i | i <- [0..]]
        childInits = map (+8) childBirths

input = [4,5,3,2,3,3,2,4,2,1,2,4,5,2,2,2,4,1,1,1,5,1,1,2,5,2,1,1,4,4,5,5,1,2,1,1,5,3,5,2,4,3,2,4,5,3,2,1,4,1,3,1,2,4,1,1,4,1,4,2,5,1,4,3,5,2,4,5,4,2,2,5,1,1,2,4,1,4,4,1,1,3,1,2,3,2,5,5,1,1,5,2,4,2,2,4,1,1,1,4,2,2,3,1,2,4,5,4,5,4,2,3,1,4,1,3,1,2,3,3,2,4,3,3,3,1,4,2,3,4,2,1,5,4,2,4,4,3,2,1,5,3,1,4,1,1,5,4,2,4,2,2,4,4,4,1,4,2,4,1,1,3,5,1,5,5,1,3,2,2,3,5,3,1,1,4,4,1,3,3,3,5,1,1,2,5,5,5,2,4,1,5,1,2,1,1,1,4,3,1,5,2,3,1,3,1,4,1,3,5,4,5,1,3,4,2,1,5,1,3,4,5,5,2,1,2,1,1,1,4,3,1,4,2,3,1,3,5,1,4,5,3,1,3,3,2,2,1,5,5,4,3,2,1,5,1,3,1,3,5,1,1,2,1,1,1,5,2,1,1,3,2,1,5,5,5,1,1,5,1,4,1,5,4,2,4,5,2,4,3,2,5,4,1,1,2,4,3,2,1]

part1 inits = sum [fishrec i 80 | i <- inits]

fishrec' f init d = 1 + sum [f i d | i <- childInits]
  where childBirths = takeWhile (<=d) [init + 1 + 7*i | i <- [0..]]
        childInits = map (+8) childBirths

fishmemo d = \i -> arr ! i
  where arr = array (0,lim) [(i,fishrec' (\i _d -> arr ! i) i d) | i <- [0..lim]]
        lim = d+8

part2 inits = sum [f i | i <- inits]
  where f = fishmemo 256

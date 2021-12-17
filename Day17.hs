module Day17 where

import Data.List

input = ((56,76),(-162,-134))

example = ((20,30),(-10,-5))

step (x,y) (velx,vely) = (x+velx,y+vely)
nextVel (velx,vely) = (velx-signum velx,vely-1)

trajectory vel = scanl' step (0,0) (iterate nextVel vel)

hit ((boxx0,boxx1),(boxy0,boxy1)) vel = any ok prefix
  where prefix = takeWhile (\(x,y) -> y>=boxy0 && x<=boxx1) $ trajectory vel
        ok (x,y) = and [x >= boxx0, x <= boxx1, y >= boxy0, y <= boxy1]

increasing as = map snd $ takeWhile (uncurry (<)) $ zip as (tail as)

maxy vel = maximum $ increasing $ ys
  where ys = map snd (trajectory vel)

part1 box = maximum [maxy (vx,vy) | vy <- [1..200], vx <- [1..50], hit box (vx,vy)]

part2 box = length [(vx,vy) | vy <- [-200..1000], vx <- [1..76], hit box (vx,vy)]

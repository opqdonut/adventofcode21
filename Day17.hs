module Day17 where

import Debug.Trace
import Data.List

type Coord = (Int,Int)
type Box = (Coord,Coord)

input :: Box
input = ((56,76),(-162,-134))

example :: Box
example = ((20,30),(-10,-5))

step :: Coord -> Coord -> Coord
step (x,y) (velx,vely) = (x+velx,y+vely)

nextVel :: Coord -> Coord
nextVel (velx,vely) = (velx-signum velx,vely-1)

data Result = Hit Int | Miss | TooHigh
  deriving (Show,Eq)

inBox :: Box -> Coord -> Bool
inBox ((boxx0,boxx1),(boxy0,boxy1)) (x,y) =
  and [x >= boxx0, x <= boxx1, y >= boxy0, y <= boxy1]

simulate box@((boxx0,boxx1),(boxy0,boxy1)) vel = go 0 vel (0,0)
  where go maxY vel (x,y)
          | x > boxx1 && y > boxy1 = TooHigh
          | y < boxy0 = Miss
          | inBox box (x,y) = Hit maxY
          | otherwise = go (max maxY y) (nextVel vel) (step (x,y) vel)

-- the trajectory is symmetric above the y=0 line, so we know we'll come down via the coordinates
-- y = vy, 0, -vy-1, -2vy-1, -3vy-1, ...

velBounds ((boxx0,boxx1),(boxy0,boxy1)) =
  ((1,boxx1),(boxy0,maxYVel))
  where maxYVel = maximum $ filter ok [1..(-boxy0)+1]
        ok y = any (<=boxy1) $ takeWhile (>= boxy0) [-k*y-1 | k<-[1..]]

hits box = go vxMin vyMin
  where ((vxMin,vxMax),(vyMin,vyMax)) = velBounds box
        go vx vy
          | vy>vyMax = []
          | vx>vxMax = go 1 (vy+1)
          | otherwise = case simulate box (vx,vy) of
                          Hit y -> y : go (vx+1) vy
                          Miss -> go (vx+1) vy
                          TooHigh -> go 1 (vy+1)

part1 box = maximum $ hits box

part2 box = length $ hits box

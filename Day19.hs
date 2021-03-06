module Day19 where

import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

--
-- A small vector library
--

type Coord = (Int,Int,Int)

dot :: Coord -> Coord -> Int
dot (a,b,c) (x,y,z) = a*x+b*y+c*z

cross, add :: Coord -> Coord -> Coord
cross (a1,a2,a3) (b1,b2,b3) = (a2*b3-a3*b2, a3*b1-a1*b3, a1*b2-a2*b1)
add (a,b,c) (x,y,z) = (a+x,b+y,c+z)

neg :: Coord -> Coord
neg (a,b,c) = (-a,-b,-c)

dist :: Coord -> Coord -> Coord
dist (x,y,z) (x',y',z') = (abs (x-x'),abs (y-y'),abs (z-z'))

-- Regularized vector distance: ignores rotations and flips
regDist :: Coord -> Coord -> Coord
regDist u v = (a,b,c)
  where (dx,dy,dz) = dist u v
        [a,b,c] = sort [dx,dy,dz]

manhattan :: Coord -> Coord -> Int
manhattan u v = dot (1,1,1) $ dist u v

-- Transform p to the coordinate system where x points to `facing` and y points to `up`
rotation :: Coord -> Coord -> Coord -> Coord
rotation facing up p = (x,y,z)
  where x = dot facing p
        y = dot up p
        z = dot (cross facing up) p

-- All the major axes and their negations
directions :: [Coord]
directions = [(1,0,0),(0,1,0),(0,0,1),(-1,0,0),(0,-1,0),(0,0,-1)]

-- All possible combinations of 90° rotations around axes
rotations :: [Coord -> Coord]
rotations = [rotation facing up | facing <- directions, up <- directions, dot up facing == 0]

--
-- The solution
--

type Scanner = [Coord]

parse :: String -> [Scanner]
parse i = map scanner parts
  where parts = splitOn [""] (lines i)
        scanner (_:coords) = map coord coords
        coord s = read $ "(" ++ s ++ ")"

input = parse <$> readFile "input.19"
example = parse <$> readFile "example.19"

-- Which rotations map {b1,b2} to {a1,a2}?
rotationsMatching a1 a2 b1 b2 = [f | f <- rotations, delta == dist (f b1) (f b2)]
  where delta = dist a1 a2

-- Which transforms map {b1,b2} to {a1,a2}?
transformsMatching a1 a2 b1 b2 =
  [ add t . r
  | r <- rotationsMatching a1 a2 b1 b2
  -- since rotations match, we can find the corresponding pair with min
  , let a = min a1 a2
        b = min (r b1) (r b2)
        t = add (neg b) a]

-- Do two scanners overlap enough?
overlaps :: Scanner -> Scanner -> Bool
overlaps sa sb = length (intersect sa sb) >= 12

-- Find the transforms f mapping {pb1,pb2} to {pa1,pa2}
-- such that f sb overlaps with sa
matching :: Coord -> Coord -> Coord -> Coord -> Scanner -> Scanner -> [Coord -> Coord]
matching pa1 pa2 pb1 pb2 sa sb =
  [ f
  | f <- transformsMatching pa1 pa2 pb1 pb2
  , overlaps sa (map f sb) ]

-- Classify pairs of points in a scanner by their regDist
-- NB! assuming regDist values are unique. Works out in practice but is a bit ugly
regDists :: Scanner -> M.Map Coord (Coord,Coord)
regDists scanner = M.fromList [ (regDist u v,(u,v))
                              | (u:rest) <- tails scanner, v <- rest]

-- Find a transform that matches sb to sa.
--
-- Avoids comparing all possible pairs of points by trying to match
-- pairs pa1,pa2 <- sa with pairs pb1,pb2 <- sb only when
--   regDist pa1 pa2 == regDist pb1 pb2
findFit :: Scanner -> Scanner -> Maybe (Coord -> Coord)
findFit sa sb = listToMaybe rets
  where ra = regDists sa
        rb = regDists sb
        candidates = M.intersectionWith (,) ra rb
        rets = [f | ((pa1,pa2),(pb1,pb2)) <- M.elems candidates
                  , f <- matching pa1 pa2 pb1 pb2 sa sb]

fitAll' origins beacons [] = (origins,beacons)
fitAll' origins beacons (scanner:scanners) =
  case findFit (S.elems beacons) scanner of
    Just f -> fitAll' (origins++[f (0,0,0)]) (S.union beacons (S.fromList (map f scanner))) scanners
    Nothing -> fitAll' origins beacons (scanners++[scanner])

-- Match all scanners together, returning scanner locations and beacon locations
fitAll :: [Scanner] -> ([Coord],S.Set Coord)
fitAll (s:ss) = fitAll' [(0,0,0)] (S.fromList s) ss

part1 ss = S.size . snd $ fitAll ss

part2 ss = maximum [manhattan o1 o2 | o1 <- origins, o2 <- origins]
  where (origins,_) = fitAll ss

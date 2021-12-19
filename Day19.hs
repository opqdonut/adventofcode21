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

-- regularized vector distance: ignores rotations and flips
regDist :: Coord -> Coord -> Coord
regDist u v = (a,b,c)
  where (dx,dy,dz) = dist u v
        [a,b,c] = sort [dx,dy,dz]

manhattan :: Coord -> Coord -> Int
manhattan u v = dot (1,1,1) $ dist u v

-- transform p to the coordinate system where x points to `facing` and y points to `up`
rotation :: Coord -> Coord -> Coord -> Coord
rotation facing up p = (x,y,z)
  where x = dot facing p
        y = dot up p
        z = dot (cross facing up) p

-- all the major axes and their negations
directions :: [Coord]
directions = [(1,0,0),(0,1,0),(0,0,1),(-1,0,0),(0,-1,0),(0,0,-1)]

-- all possible combinations of 90° rotations around axes
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

-- which rotations map {b1,b2} to {a1,a2}?
rotationsMatching a1 a2 b1 b2 = [f | f <- rotations, delta == dist (f b1) (f b2)]
  where delta = dist a1 a2

-- the translation that maps pb to pa
findTranslation pa pb = add trans
  where trans = (add (neg pb) pa)

-- do two scanners overlap enough?
overlaps :: Scanner -> Scanner -> Bool
overlaps sa sb = length (intersect sa sb) >= 12

-- find the transforms f mapping {pb1,pb2} to {pa1,pa2}
-- such that f sb overlaps with sa
matching :: Coord -> Coord -> Coord -> Coord -> Scanner -> Scanner -> [Coord -> Coord]
matching pa1 pa2 pb1 pb2 sa sb = rets
  where rots = rotationsMatching pa1 pa2 pb1 pb2
        rets = [ trans . rot
               | rot <- rots
               , let pb1' = rot pb1
               , let pb2' = rot pb2
               , let sb' = map rot sb
               , pb' <- [pb1',pb2']
               , let trans = findTranslation pa1 pb'
               , let sb'' = map trans sb'
               , overlaps sa sb'' ]

-- Classify pairs of points in a scanner by theyr regDist
-- NB! assuming regDist values are unique. Works out in practice but is a bit ugly
regDists :: Scanner -> M.Map Coord (Coord,Coord)
regDists scanner = M.fromList [ (regDist u v,(u,v))
                              | (u:rest) <- tails scanner, v <- rest]

-- find a transform that matches sb to sa
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

-- match all scanners together, returning scanner locations and beacon locations
fitAll :: [Scanner] -> ([Coord],S.Set Coord)
fitAll (s:ss) = fitAll' [(0,0,0)] (S.fromList s) ss

part1 ss = S.size . snd $ fitAll ss

part2 ss = maximum [manhattan o1 o2 | o1 <- origins, o2 <- origins]
  where (origins,_) = fitAll ss

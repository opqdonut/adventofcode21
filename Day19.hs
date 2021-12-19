module Day19 where

import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

type Coord = (Int,Int,Int)
type Scanner = [Coord]

parse :: String -> [Scanner]
parse i = map scanner parts
  where parts = splitOn [""] (lines i)
        scanner (_:coords) = map coord coords
        coord s = read $ "(" ++ s ++ ")"

input = parse <$> readFile "input.19"
example = parse <$> readFile "example.19"

dist (x,y,z) (x',y',z') = (abs (x-x'),abs (y-y'),abs (z-z'))

regDist :: Coord -> Coord -> Coord
regDist u v = (a,b,c)
  where (dx,dy,dz) = dist u v
        [a,b,c] = sort [dx,dy,dz]

dot :: Coord -> Coord -> Int
dot (a,b,c) (x,y,z) = a*x+b*y+c*z

cross, add :: Coord -> Coord -> Coord
cross (a1,a2,a3) (b1,b2,b3) = (a2*b3-a3*b2, a3*b1-a1*b3, a1*b2-a2*b1)
add (a,b,c) (x,y,z) = (a+x,b+y,c+z)

neg :: Coord -> Coord
neg (a,b,c) = (-a,-b,-c)

rotation facing up p = (x,y,z)
  where x = dot facing p
        y = dot up p
        z = dot (cross facing up) p

directions = [(1,0,0),(0,1,0),(0,0,1),(-1,0,0),(0,-1,0),(0,0,-1)]

rotations = [rotation facing up | facing <- directions, up <- directions, dot up facing == 0]

rotationsMatching a1 a2 b1 b2 = [f | f <- rotations, delta == dist (f b1) (f b2)]
  where delta = dist a1 a2

fitTrans pa pb sa sb = sbFixed
  where trans = (add (neg pb) pa)
        sbFixed = map (add trans) sb

fits sa sb = length (intersect sa sb) >= 12

fit pa1 pa2 pb1 pb2 sa sb = rets
  where rots = rotationsMatching pa1 pa2 pb1 pb2
        rets = [ fitted
               | rot <- rots
               , let pb1' = rot pb1
               , let pb2' = rot pb2
               , let sb' = map rot sb
               , pb' <- [pb1',pb2']
               , let fitted = fitTrans pa1 pb' sa sb'
               , fits sa fitted ]

{-
*Day19> e <- example
*Day19> [s0,s1] = take 2 e
*Day19> s1 = e!!1
*Day19> interesting 1 s0
[((1,81,163),(-537,-823,-458),(-618,-824,-621))]
*Day19> interesting 1 s1
[((1,81,163),(686,422,578),(605,423,415))]
*Day19> fit (-537,-823,-458) (-618,-824,-621) (686,422,578) (605,423,415) s0 s1
-}

regDists :: Scanner -> M.Map Coord (Coord,Coord)
regDists scanner = M.fromList [ (regDist u v,(u,v))
                              | (u:rest) <- tails scanner, v <- rest]

findFit sa sb = listToMaybe rets
  where ra = regDists sa
        rb = regDists sb
        candidates = M.intersectionWith (,) ra rb
        rets = [union sa sb' | ((pa1,pa2),(pb1,pb2)) <- M.elems candidates
                             , sb' <- fit pa1 pa2 pb1 pb2 sa sb]

fitAll state [] = state
fitAll state (scanner:scanners) = case findFit state scanner of
                                    Just state' -> fitAll state' scanners
                                    Nothing -> fitAll state (scanners++[scanner])

part1 (s:ss) = fitAll s ss

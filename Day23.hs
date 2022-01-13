module Day23 where

import Data.List
import Data.Maybe
import Control.Monad.State
import Debug.Trace
import qualified Data.Map as M
import qualified Data.Set as S

-- Modeling the field

-- x0123456789A
--y#############
--0#...........#
--1###a#b#c#d###
--2  #a#b#c#d#
--   #########

type Coord = (Int,Int)

neigh :: Coord -> [Coord]
neigh (0,0) = [(1,0)]
neigh (10,0) = [(9,0)]
neigh (x,0)
  | even x = [(x,1),(x-1,0),(x+1,0)]
  | otherwise = [(x-1,0),(x+1,0)]
neigh (x,1) = [(x,0),(x,2)]
neigh (x,2) = [(x,1)]

manhattan (x0,y0) (x1,y1) = abs (x0-x1) + abs (y0-y1)

data Color = A|B|C|D
  deriving (Show,Eq,Enum,Ord)

cost A = 1
cost B = 10
cost C = 100
cost D = 1000

homeCol A = 2
homeCol B = 4
homeCol C = 6
homeCol D = 8

home ((x,y),col) = x == homeCol col && y>0

room (x,y) = y>0

hallway (x,y) = y==0 && not (elem x [2,4,6,8])

maxY = 4 --2 for part1

type GameState = M.Map Coord Color

reachableHallways :: GameState -> Coord -> [Coord]
reachableHallways state (x,y)
  | gotOut = walk x (-1) ++ walk x 1
  | otherwise = []
  where free c = not (M.member c state)
        gotOut = all free [(x,y') | y' <- [1..y-1]]
        walk x delta
          | x < 0 || x > 10 = []
          | not (free (x,0)) = []
          | not (hallway (x,0)) = walk (x+delta) delta
          | otherwise = (x,0):walk (x+delta) delta

moveOut :: GameState -> (Coord,Color) -> [((Coord,Color),Int)]
moveOut state (u,col) = [ ((v,col),cost col * manhattan u v)
                        | v <- reachableHallways state u ]

reachableHomes state ((x,0),col)
  | gotIn = takeWhile free [(home,y') | y' <- [1..maxY]]
  | otherwise = []
  where free c = not (M.member c state)
        home = homeCol col
        delta = signum (home - x)
        range a b = [min a b..max a b]
        gotIn = all free [(x',0) | x' <- range (x+delta) home]

moveIn :: GameState -> (Coord,Color) -> [((Coord,Color),Int)]
moveIn state (u,col)
  | notFree = []
  | otherwise = take 1 . reverse . sort $ homes -- prefer largest y
    where notFree = or [home (v,col) | (v,col') <- M.assocs state, col/=col']
          occupied x = M.member x state
          homes = [ ((v,col),cost col * manhattan u v)
                  | v <- reachableHomes state (u,col) ]

finished state ((x,0),col) = False
finished state ((x,y),col) = home ((x,y),col) && all ok [(x,y') | y' <- [y+1..maxY]]
  where ok c = M.lookup c state == Just col

move :: GameState -> (Coord,Color) -> [((Coord,Color),Int)]
move state (u,col)
  | finished state (u,col) = []
  | hallway u = moveIn state (u,col)
  | otherwise = moveOut state (u,col)

pick m = [((k,v),M.delete k m) | (k,v) <- M.assocs m]

minimumMaybe [] = Nothing
minimumMaybe xs = Just (minimum xs)

estimate state = sum $ map finish todo
  where todo = filter (not.home) $ M.assocs state
        finish ((x,y),col) = cost col * (abs (x-homeCol col)+1)

assoc (k,v) m = M.insert k v m

minSol :: GameState -> Int
minSol state0 = execState (go 0 state0) 999999999
  where check d op = do
          lim <- get
          when (d<lim) op
        go :: Int -> GameState -> State Int ()
        go d state
          | all home (M.assocs state) = trace ("found "++show d) $ modify (min d)
          | otherwise =
              sequence_ [ check (d+cost+estimate state') $ go (d+cost) state'
                        | (target,rest) <- pick state
                        , (next,cost) <- move state target
                        , let state' = assoc next rest ]

build = M.fromList . map (\(a,b) -> (b,a))

-- ###2#4#6#8###
--0#...........#
--1###A#D#C#A###
--2  #C#D#B#B#
--   #########
input :: GameState
input = build
        [(A,(2,1)),(C,(2,2))
        ,(D,(4,1)),(D,(4,2))
        ,(C,(6,1)),(B,(6,2))
        ,(A,(8,1)),(B,(8,2))]

example :: GameState
example = build
          [(B,(2,1)),(A,(2,2))
          ,(C,(4,1)),(D,(4,2))
          ,(B,(6,1)),(C,(6,2))
          ,(D,(8,1)),(A,(8,2))]

example' :: GameState
example' = build
           [(D,(5,0))
           ,(A,(2,2))
           ,(B,(4,1)),(B,(4,2))
           ,(C,(6,1)),(C,(6,2))
           ,(D,(8,1)),(A,(8,2))]

example'' :: GameState
example'' = build
            [(D,(5,0))
            ,(D,(7,0))
            ,(A,(2,2))
            ,(B,(4,1)),(B,(4,2))
            ,(C,(6,1)),(C,(6,2))
            ,(A,(8,2))]

simple :: GameState
simple = build
         [(B,(2,1)),(A,(2,2))
         ,(C,(4,1)),(D,(4,2))
         ,(B,(6,1)),(C,(6,2))
         ,(A,(8,1)),(D,(8,2))]

-- part2
input2 :: GameState
input2 = build
         [(A,(2,1)),(D,(2,2)),(D,(2,3)),(C,(2,4))
         ,(D,(4,1)),(C,(4,2)),(B,(4,3)),(D,(4,4))
         ,(C,(6,1)),(B,(6,2)),(A,(6,3)),(B,(6,4))
         ,(A,(8,1)),(A,(8,2)),(C,(8,3)),(B,(8,4))]

example2 :: GameState
example2 = build
          [(B,(2,1)),(D,(2,2)),(D,(2,3)),(A,(2,4))
          ,(C,(4,1)),(C,(4,2)),(B,(4,3)),(D,(4,4))
          ,(B,(6,1)),(B,(6,2)),(A,(6,3)),(C,(6,4))
          ,(D,(8,1)),(A,(8,2)),(C,(8,3)),(A,(8,4))]

simple2 :: GameState
simple2 = build [(B,(2,1)),(A,(2,2)),(A,(2,3)),(A,(2,4))
                ,(C,(4,1)),(D,(4,2)),(B,(4,3)),(B,(4,4))
                ,(B,(6,1)),(C,(6,2)),(C,(6,3)),(C,(6,4))
                ,(A,(8,1)),(D,(8,2)),(D,(8,3)),(D,(8,4))]

swap2 :: GameState
swap2 = build [(B,(2,1)),(B,(2,2)),(B,(2,3)),(B,(2,4))
              ,(A,(4,1)),(A,(4,2)),(A,(4,3)),(A,(4,4))
              ,(C,(6,1)),(C,(6,2)),(C,(6,3)),(C,(6,4))
              ,(D,(8,1)),(D,(8,2)),(D,(8,3)),(D,(8,4))]

swap4 :: GameState
swap4 = build [(B,(2,1)),(B,(2,2)),(B,(2,3)),(B,(2,4))
              ,(A,(4,1)),(A,(4,2)),(A,(4,3)),(A,(4,4))
              ,(D,(6,1)),(D,(6,2)),(D,(6,3)),(D,(6,4))
              ,(C,(8,1)),(C,(8,2)),(C,(8,3)),(C,(8,4))]

part2 = minSol

main = print $ part2 input2

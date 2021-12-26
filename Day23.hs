module Day23 where

import Data.List
import Data.Maybe
import Control.Monad.State
import Debug.Trace

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

allMoves :: (Coord -> Bool) -> Coord -> [(Coord,Int)]
allMoves occupied x = visit [x] 0 x
  where visit visited dist c =
          let nexts = filter (not.occupied) . filter (not.(`elem`visited)) $ neigh c
          in (c,dist):concatMap (visit (c:visited) (dist+1)) nexts

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

home (col,(x,y)) = x == homeCol col && y>0

room (x,y) = y>0

hallway (x,y) = y==0 && not (elem x [2,4,6,8])

type GameState = [(Color,Coord)]

moveOut :: GameState -> (Color,Coord) -> [((Color,Coord),Int)]
moveOut state (col,u) = [ ((col,v),cost col * dist)
                        | (v,dist) <- allMoves occupied u
                        , hallway v ]
  where poss = map snd state
        occupied x = elem x poss

moveIn :: GameState -> (Color,Coord) -> [((Color,Coord),Int)]
moveIn state (col,u)
  | notFree = []
  | otherwise = take 1 . reverse . sort $ homes -- prefer largest y
    where notFree = or [home (col,v) | (col',v) <- state, col/=col']
          occupied x = elem x (map snd state)
          homes = [ ((col,v),cost col * dist)
                  | (v,dist) <- allMoves occupied u
                  , home (col,v) ]

finished state (col,(x,0)) = False
finished state (col,(x,2)) = home (col,(x,2))
finished state (col,(x,1)) = home (col,(x,1)) && elem (col,(x,2)) state

move :: GameState -> (Color,Coord) -> [((Color,Coord),Int)]
move state (col,u)
  | finished state (col,u) = []
  | hallway u = moveIn state (col,u)
  | otherwise = moveOut state (col,u)

pick xs = [(x,delete x xs) | x <- xs]

minimumMaybe [] = Nothing
minimumMaybe xs = Just (minimum xs)

estimate state = sum $ map finish todo
  where todo = filter (not.home) state
        finish (col,(x,y)) = cost col * (abs (x-homeCol col)+1)

minSol :: GameState -> Int
minSol state0 = execState (go 0 state0) 999999999
  where check d op = do
          lim <- get
          when (d<lim) op
        go :: Int -> GameState -> State Int ()
        go d state
          | all home state = trace ("found "++show d) $ modify (min d)
          | otherwise =
              sequence_ [ check (d+cost+estimate state') $ go (d+cost) state'
                        | (target,rest) <- pick $ reverse $ sort state -- try to move expensive first
                        , (next,cost) <- move state target
                        , let state' = next:rest ]
-- ###2#4#6#8###
--0#...........#
--1###A#D#C#A###
--2  #C#D#B#B#
--   #########
input :: GameState
input = [(A,(2,1)),(C,(2,2))
        ,(D,(4,1)),(D,(4,2))
        ,(C,(6,1)),(B,(6,2))
        ,(A,(8,1)),(B,(8,2))]

example :: GameState
example = [(B,(2,1)),(A,(2,2))
          ,(C,(4,1)),(D,(4,2))
          ,(B,(6,1)),(C,(6,2))
          ,(D,(8,1)),(A,(8,2))]

example' :: GameState
example' = [(D,(5,0))
           ,(A,(2,2))
           ,(B,(4,1)),(B,(4,2))
           ,(C,(6,1)),(C,(6,2))
           ,(D,(8,1)),(A,(8,2))]

example'' :: GameState
example'' = [(D,(5,0))
            ,(D,(7,0))
            ,(A,(2,2))
            ,(B,(4,1)),(B,(4,2))
            ,(C,(6,1)),(C,(6,2))
            ,(A,(8,2))]

simple :: GameState
simple = [(B,(2,1)),(A,(2,2))
         ,(C,(4,1)),(D,(4,2))
         ,(B,(6,1)),(C,(6,2))
         ,(A,(8,1)),(D,(8,2))]


part1 = minSol

main = print $ part1 input

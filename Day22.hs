module Day22 where

import Data.List
import qualified Data.Set as S
import Data.List.Split
import Control.DeepSeq

type Coord = (Int,Int,Int)
type Range = (Int,Int)
type Rule = (Bool,(Range,Range,Range))

parseLine :: String -> Rule
parseLine l = (state=="on",(limx,limy,limz))
  where [state,limits] = words l
        parseRange r = let [a,b] = splitOn ".." $ drop 2 r
                       in (read a, read b)
        [limx,limy,limz] = map parseRange $ splitOn "," limits

parse = map parseLine . lines

example = parse <$> readFile "example.22"
example2 = parse <$> readFile "example2.22"
input = parse <$> readFile "input.22"

range a0 a1
  | a1 < -50 = []
  | a0 > 50 = []
  | otherwise = [max (-50) a0..min 50 a1]

apply :: S.Set Coord -> Rule -> S.Set Coord
apply s (b,((x0,x1),(y0,y1),(z0,z1))) = foldl' f s coords
  where coords = [(x,y,z) | x <- range x0 x1
                          , y <- range y0 y1
                          , z <- range z0 z1]
        f = flip $ if b then S.insert else S.delete

part1 i = S.size $ foldl' apply S.empty i

--- part 2 ---

valid (a0,a1) = a0 <= a1

rangeDiff :: Range -> Range -> [Range]
rangeDiff (a0,a1) (b0,b1)
  | b0 > a1 = [(a0,a1)]
  | b1 < a0 = [(a0,a1)]
  | otherwise = filter valid [(a0,b0-1),(b1+1,a1)]

rangeIntersect :: Range -> Range -> [Range]
rangeIntersect (a0,a1) (b0,b1)
  | b0 > a1 = []
  | b1 < a0 = []
  | otherwise = [(max a0 b0, min a1 b1)]

rangeSize (a0,a1) = a1 + 1 - a0

type Cube = (Range,Range,Range)

cubeSize (x,y,z) = rangeSize x * rangeSize y * rangeSize z

cubeDiff :: Cube -> Cube -> [Cube]
cubeDiff (ax,ay,az) (bx,by,bz) = topBot ++ leftRight ++ frontBack
  where xPieces = rangeDiff ax bx
        yPieces = rangeDiff ay by
        zPieces = rangeDiff az bz
        topBot = [(ax,ay,az') | az' <- zPieces]
        leftRight = [(ax',ay,bz') | ax' <- xPieces, bz' <- rangeIntersect bz az]
        frontBack = [(bx',ay',bz') | ay' <- yPieces
                                   , bz' <- rangeIntersect bz az
                                   , bx' <- rangeIntersect bx ax]

cubeIntersect :: Cube -> Cube -> [Cube]
cubeIntersect (ax,ay,az) (bx,by,bz) =
  [ (x,y,z)
  | x <- rangeIntersect ax bx
  , y <- rangeIntersect ay by
  , z <- rangeIntersect az bz]

rmCube :: Cube -> [Cube] -> [Cube]
rmCube off [] = []
rmCube off (c:cs) = force $ cubeDiff c off ++ rmCube off cs

addCube :: Cube -> [Cube] -> [Cube]
addCube new cs = force $ go [new] cs
  where go news [] = news
        go news (c:cs) = c:go (concatMap (\new -> cubeDiff new c) news) cs

applyRule :: [Cube] -> Rule -> [Cube]
applyRule cs (True,c) = addCube c cs
applyRule cs (False,c) = rmCube c cs

-- make it faster by delaying addCube operations until the end
applyRule' :: [Cube] -> Rule -> [Cube]
applyRule' cs (True,c) = c:cs
applyRule' cs (False,c) = rmCube c cs

applyAll :: [Rule] -> [Cube]
applyAll rs = finish $ foldl' applyRule' [] rs
  where finish cs = foldl' (flip addCube) [] cs

part1' rs = sum $ map cubeSize $ concatMap (cubeIntersect region) $ foldl applyRule [] rs
  where region = ((-50,50),(-50,50),(-50,50))

part1'' rs = sum $ map cubeSize $ concatMap (cubeIntersect region) $ applyAll rs
  where region = ((-50,50),(-50,50),(-50,50))

part2 rs = sum $ map cubeSize $ foldr (flip applyRule) [] (reverse rs)

part2' rs = sum $ map cubeSize $ applyAll rs

-- stack ghc -- --make -main-is Day22 -O2 Day22.hs
main = do
  i <- input
  print $ part2' i

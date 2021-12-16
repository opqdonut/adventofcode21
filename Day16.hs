module Day16 where

import Control.Monad
import Control.Monad.State
import Data.Char
import Data.List

input = readFile "input.16"

hexToBits :: String -> [Int]
hexToBits = concatMap (h.digitToInt)
  where b x = mod x 2
        h x = map (b . div x) [8,4,2,1]

bitsToInt :: [Int] -> Int
bitsToInt = foldl' (\v b -> 2*v + b) 0

data Packet = Literal {version :: Int, lit :: Int}
            | Operator {version :: Int, typ :: Int, subs :: [Packet]}
  deriving Show

bits :: Int -> State [Int] [Int]
bits n = do
  bs <- gets (take n)
  modify (drop n)
  return bs

int n = bitsToInt <$> bits n

parse :: State [Int] Packet
parse = do
  version <- int 3
  typ <- int 3
  case typ of 4 -> literal version
              _ -> operator typ version

parseAll = do
  end <- gets null
  if end
    then return []
    else liftM2 (:) parse parseAll

intLiteral = do
  ~(b:payload) <- bits 5
  case b of 0 -> return payload
            1 -> (payload++) <$> intLiteral

literal version = Literal version . bitsToInt <$> intLiteral

operator typ version = do
  ~[lengthType] <- bits 1
  subs <- case lengthType of 0 -> int 15 >>= subsBits
                             1 -> int 11 >>= subsCount
  return $ Operator version typ subs

subsBits n = do
  now <- bits n
  rest <- get
  put now
  subs <- parseAll
  put rest
  return subs

subsCount n = replicateM n parse

sumVersions (Literal v _) = v
sumVersions (Operator v _ subs) = v + sum (map sumVersions subs)

part1 i = sumVersions p
  where p = evalState parse (hexToBits i)

evaluate (Literal _ i) = i
evaluate (Operator _ t subs) = operate t (map evaluate subs)

operate 0 d = sum d
operate 1 d = product d
operate 2 d = minimum d
operate 3 d = maximum d
-- 4 = literal
operate 5 [x,y] = if x>y then 1 else 0
operate 6 [x,y] = if x<y then 1 else 0
operate 7 [x,y] = if x==y then 1 else 0

part2 i = evaluate p
  where p = evalState parse (hexToBits i)

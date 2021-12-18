module Day18 where

import Debug.Trace
import Data.Char
import Control.Applicative
import Data.Maybe

data P = N Int | P P P
  deriving (Show,Eq)

parseP ('[':rest) = (P l r, rest'')
  where (l,',':rest') = parseP rest
        (r,']':rest'') = parseP rest'
parseP s = (N (read digits), rest)
  where (digits,rest) = span isDigit s

parse s = case parseP s of (p,"") -> p

parseAll = map parse . lines

unparse (N i) = show i
unparse (P l r) = "[" ++ unparse l ++ "," ++ unparse r ++ "]"

mid (_,m,_) = m

explode :: P -> Maybe P
explode p = mid <$> explode' 0 p

explode' :: Int -> P -> Maybe (Maybe Int, P, Maybe Int)
explode' _ (N _) = Nothing
explode' 4 (P (N l) (N r)) = Just (Just l,N 0,Just r)
explode' i (P l r) = (reconstructL <$> explode' (i+1) l <*> Just r)
                     <|>
                     (reconstructR <$> Just l <*> explode' (i+1) r)

reconstructL :: (Maybe Int, P, Maybe Int) -> P -> (Maybe Int, P, Maybe Int)
reconstructL (mi,l,mj) r = (mi,P l (addLeft mj r),Nothing)

reconstructR :: P -> (Maybe Int, P, Maybe Int) -> (Maybe Int, P, Maybe Int)
reconstructR l (mi,r,mj) = (Nothing,P (addRight mi l) r,mj)

flop :: P -> P
flop (P l r) = P (flop r) (flop l)
flop n = n

addLeft :: Maybe Int -> P -> P
addLeft Nothing p = p
addLeft (Just i) (N n) = N (n+i)
addLeft mi (P l r) = P (addLeft mi l) r

addRight :: Maybe Int -> P -> P
addRight mi p = flop (addLeft mi (flop p))

split (N i)
  | i >= 10 = Just $ P (N l) (N (i-l))
  | otherwise = Nothing
  where l = div i 2
split (P l r) = (P <$> split l <*> Just r)
                <|>
                (P <$> Just l <*> split r)

reduce1 p = explode p <|> split p

reduce p = case reduce1 p of Nothing -> p
                             Just p' -> reduce p'

add l r = reduce (P l r)

magnitude (N i) = i
magnitude (P l r) = 3 * magnitude l + 2 * magnitude r

part1 i = magnitude $ foldl1 add i

input = parseAll <$> readFile "input.18"
example1 = parseAll <$> readFile "example1.18"
example2 = parseAll <$> readFile "example2.18"

part2 i = maximum [magnitude (add x y) | x <- i, y <- i, x /= y]

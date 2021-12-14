module Day14 where

import Data.List
import Control.Monad.State
import qualified Data.Map as M

parse :: String -> (String,[(String,Char)])
parse i = (formula,rules)
  where (formula:_:rules0) = lines i
        rules = map r rules0
        r l = case words l of [f,_,[t]] -> (f,t)

input = parse <$> readFile "input.14"
input2 = parse <$> readFile "input2.14" -- part1 2170
example = parse <$> readFile "example.14"

replace :: [(String,Char)] -> String -> String
replace rules (a:b:xs) =
  case lookup [a,b] rules of Just c -> a:c:rest
                             Nothing -> a:rest
  where rest = replace rules (b:xs)
replace _ s = s

freqs = map length . group . sort

part1 (formula,rules) = maximum fs - minimum fs
  where final = iterate (replace rules) formula !! 10
        fs = freqs final

p1 (formula,rules) n = maximum fs - minimum fs
  where final = iterate (replace rules) formula !! n
        fs = freqs final


count :: [(String,Char)] -> Int -> String -> State (M.Map (Int,String) (M.Map Char Integer)) (M.Map Char Integer)
count rules 0 [a,b] = return M.empty
count rules n [a,b] = do
  memo <- get
  case M.lookup (n,[a,b]) memo of
    Just res -> return res
    Nothing -> case lookup [a,b] rules
      of Just c -> do left <- count rules (n-1) [a,c]
                      right <- count rules (n-1) [c,b]
                      let res = M.unionsWith (+) [left,right,M.singleton c 1]
                      memo <- get
                      put (M.insert (n,[a,b]) res memo)
                      return res
         Nothing -> return M.empty

countAll rules n s = do
  insertions <- mapM (\(a,b) -> count rules n [a,b]) (zip s (tail s))
  let base = M.fromListWith (+) (zip s (repeat 1))
  return $ M.unionsWith (+) (base:insertions)

part2 (formula,rules) = maximum fs - minimum fs
  where final = evalState (countAll rules 40 formula) M.empty
        fs = M.elems final

p2 (formula,rules) n = maximum fs - minimum fs
  where final = evalState (countAll rules n formula) M.empty
        fs = M.elems final

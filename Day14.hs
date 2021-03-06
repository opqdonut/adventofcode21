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

type Freqs = (M.Map Char Integer)

-- count characters inserted between a and b in n steps
count :: [(String,Char)] -> Int -> String -> State (M.Map (Int,String) Freqs) Freqs
count rules 0 [a,b] = return M.empty
count rules n [a,b] = do
  query <- gets (M.lookup (n,[a,b]))
  case query of
    Just res -> return res
    Nothing -> case lookup [a,b] rules
      of Nothing -> return M.empty
         Just c -> do left <- count rules (n-1) [a,c]
                      right <- count rules (n-1) [c,b]
                      let res = M.unionsWith (+) [left,right,M.singleton c 1]
                      modify (M.insert (n,[a,b]) res)
                      return res

-- count all characters when starting from s after n steps
countAll rules n s = do
  insertions <- mapM (\(a,b) -> count rules n [a,b]) (zip s (tail s))
  let base = M.fromListWith (+) (zip s (repeat 1))
  return $ M.unionsWith (+) (base:insertions)

part2 (formula,rules) = maximum fs - minimum fs
  where final = evalState (countAll rules 40 formula) M.empty
        fs = M.elems final

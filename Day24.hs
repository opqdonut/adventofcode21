module Day24 where

import Control.Monad
import Debug.Trace
import Data.Char
import Data.List
import qualified Data.Map as M

data Register = X|Y|Z|W
  deriving (Show,Read,Eq,Ord,Enum)

data Arg = Lit Int | Reg Register
  deriving (Show,Read,Eq)

data BinOp = Add|Mul|Div|Mod|Eql
  deriving (Show,Read,Eq)

data Instr = Inp Register
           | Bin BinOp Register Arg
  deriving (Show,Read,Eq)

parseRegister "x" = X
parseRegister "y" = Y
parseRegister "z" = Z
parseRegister "w" = W

parseArg s
  | all isAlpha s = Reg (parseRegister s)
  | otherwise = Lit (read s)

parseInstr s = case words s of
  ["inp",x] -> Inp (parseRegister x)
  [op,x,y] -> Bin (p op) (parseRegister x) (parseArg y)
    where p "add" = Add
          p "mul" = Mul
          p "div" = Div
          p "mod" = Mod
          p "eql" = Eql

parse = map parseInstr . lines

input = parse <$> readFile "input.24"

type Mem = M.Map Register Int

initialMem :: Mem
initialMem = M.fromList [(X,0),(Y,0),(Z,0),(W,0)]

type State = (Mem,[Int])

evalOp :: BinOp -> Int -> Int -> Int
evalOp Add = (+)
evalOp Mul = (*)
evalOp Div = div
evalOp Mod = mod
evalOp Eql = \x y -> if x==y then 1 else 0

eval :: State -> Instr -> State
eval (mem,i:is) (Inp reg) = traceShow mem $ (M.insert reg i mem, is)
eval (mem,is) (Bin op reg arg) = (M.insert reg out mem, is)
  where in1 = mem M.! reg
        in2 = case arg of Lit i -> i
                          Reg r -> mem M.! r
        out = evalOp op in1 in2

run :: [Instr] -> [Int] -> Mem
run instrs code = fst $ foldl' eval (initialMem,code) instrs

checkCode :: [Instr] -> [Int] -> Bool
checkCode instrs code = run instrs code M.! Z == 0

digit = [9,8..1]
validate x = guard (x>=1 && x<=9) >> return x
genCodes = do
  d1 <- digit -- d14
  d2 <- digit -- d13
  d3 <- digit -- d12
  d4 <- digit -- d5
  d5 <- validate $ d4+8
  d6 <- digit -- d11
  d7 <- digit -- d8
  d8 <- validate $ d7-2
  d9 <- digit -- d10
  d10 <- validate $ d9+7
  d11 <- validate $ d6
  d12 <- validate $ d3-5
  d13 <- validate $ d2+2
  d14 <- validate $ d1-4
  return [d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14]

render = concatMap show

part1 = render $ head genCodes

part2 = render $ last genCodes

module Day8 where

import Data.List

parseLine l = (map sort $ take 10 w, map sort $ drop 11 w)
  where w = words l

input = map parseLine . lines <$> readFile "input.8"

part1 i =
  length
  . filter ok
  . concat
  $ map snd i
  where ok s = length s `elem` [2,4,3,7]

--

correct :: [(String,Int)]
correct = [("abcefg",0)
          ,("cf",1)
          ,("acdeg",2)
          ,("acdfg",3)
          ,("bcdf",4)
          ,("abdfg",5)
          ,("abdefg",6)
          ,("acf",7)
          ,("abcdefg",8)
          ,("abcdfg",9)]

valids = map fst correct

apply mapping = mapM (`lookup`mapping)

plausible examples mapping = all (`elem`valids) mapped
  where mapped = [sort s | Just s <- map (apply mapping) examples]

allVars = "abcdefg"

findMapping examples = head $ go examples allVars []

go examples [] mapping = [mapping]
go examples (t:todos) mapping = concatMap (go examples todos) plau
  where available = allVars \\ map snd mapping
        mappings = [(t,x):mapping | x<-available]
        plau = filter (plausible examples) mappings

example = parseLine "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"

solveRow (examples,output) = foldl' (\s d -> 10*s+d) 0 digits
  where digits = [d | Just pat <- map (apply mapping) output, let Just d = lookup (sort pat) correct]
        mapping = findMapping examples

part2 = sum . map solveRow

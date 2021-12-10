module Day10 where

import Data.List

input = lines <$> readFile "input.10"

open c = elem c "([{<"

close '(' = ')'
close '[' = ']'
close '{' = '}'
close '<' = '>'

firstIllegal cs = go [] cs
  where go _ [] = Nothing
        go stack (c:cs)
          | open c = go (c:stack) cs
        go (s:stack) (c:cs)
          | close s == c = go stack cs
        go _ (c:_) = Just c

score (Just ')') = 3
score (Just ']') = 57
score (Just '}') = 1197
score (Just '>') = 25137
score _ = 0


part1 = sum . map (score.firstIllegal)

closers cs = go [] cs
  where go stack [] = map close stack
        go stack (c:cs)
          | open c = go (c:stack) cs
        go (s:stack) (c:cs)
          | close s == c = go stack cs
        go _ (c:_) = []

score2 = foldl' (\s c -> s*5 + f c) 0
  where f ')' = 1
        f ']' = 2
        f '}' = 3
        f '>' = 4

part2 :: [String] -> Int
part2 i = sort valids !! (length valids `div` 2)
  where valids = filter (0<).map (score2.closers) $ i

example =
  ["[({(<(())[]>[[{[]{<()<>>"
  ,"[(()[<>])]({[<{<<[]>>("
  ,"{([(<{}[<>[]}>{[]{[(<()>"
  ,"(((({<>}<{<{<>}{[]{[]{}"
  ,"[[<[([]))<([[{}[[()]]]"
  ,"[{[{({}]{}}([{[{{{}}([]"
  ,"{<[[]]>}<{[{[{[]{()[[[]"
  ,"[<(<(<(<{}))><([]([]()"
  ,"<{([([[(<>()){}]>(<<{{"
  ,"<{([{{}}[<[[[<>{}]]]>[]]"]

module Main (main) where

import Day1 (day1a, day1b)
import Day2 (day2a, day2b)
import Day3 (day3a, day3b)
import Day4 (day4a, day4b)
import System.Environment (getArgs)

solution :: String -> IO ()
solution "1a" = day1a
solution "1b" = day1b
solution "2a" = day2a
solution "2b" = day2b
solution "3a" = day3a
solution "3b" = day3b
solution "4a" = day4a
solution "4b" = day4b
solution s = error ("No solution for " <> s)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [day] -> solution day
    _ -> error "usage: ./aoc2023 <day>"
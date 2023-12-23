module Main (main) where

import Day1 (day1a, day1b)
import Day2 (day2a, day2b)
import Day3 (day3a, day3b)
import Day4 (day4a, day4b)
import Day5 (day5a, day5b)
import Day6 (day6a, day6b)
import Day7 (day7a, day7b)
import Day8 (day8a, day8b)
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
solution "5a" = day5a
solution "5b" = day5b
solution "6a" = day6a
solution "6b" = day6b
solution "7a" = day7a
solution "7b" = day7b
solution "8a" = day8a
solution "8b" = day8b
solution s = error ("No solution for " <> s)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [day] -> solution day
    _ -> error "usage: ./aoc2023 <day>"
module Main (main) where

import Day1 (day1a, day1b)
import System.Environment (getArgs)

solution :: String -> IO ()
solution "1a" = day1a
solution "1b" = day1b
solution s = error ("No solution for " <> s)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [day] -> solution day
    _ -> error "usage: ./aoc2023 <day>"
module Day2 (day2a, day2b) where

import Data.Map (Map)
import Data.Map qualified as Map (assocs, empty, fromList, lookup, unionWith)
import Lib (readLines, splitOn, strip)

type Counts = Map String Int

type Game = [Counts]

readPresentation :: String -> (String, Int)
readPresentation str = case (words . strip) str of
  [countStr, color] -> (color, read countStr)
  _ -> error $ "Invalid presentation " <> strip str

readRound :: String -> Counts
readRound = Map.fromList . map readPresentation . splitOn ','

readGame :: String -> Game
readGame str = case splitOn ':' str of
  [_, roundsStr] -> map readRound (splitOn ';' roundsStr)
  _ -> error "Invalid game"

presPossible :: Counts -> (String, Int) -> Bool
presPossible with (color, count) = case color `Map.lookup` with of
  Just have -> count <= have
  Nothing -> False

roundPossible :: Counts -> Counts -> Bool
roundPossible with = all (presPossible with) . Map.assocs

gamePossible :: Counts -> Game -> Bool
gamePossible with = all (roundPossible with)

inventory :: Counts
inventory = Map.fromList [("red", 12), ("green", 13), ("blue", 14)]

day2a :: IO ()
day2a = readLines >>= print . sum . map fst . filter (gamePossible inventory . snd) . zip [1 :: Int ..] . map readGame

fewestCubes :: Game -> Counts
fewestCubes = foldr merge Map.empty
  where
    merge = Map.unionWith max

power :: Game -> Int
power = product . fewestCubes

day2b :: IO ()
day2b = readLines >>= print . sum . map (power . readGame)
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day6 (day6a, day6b) where

import Data.Char (isDigit)
import GHC.Float (ceilingDouble, floorDouble, int2Double, sqrtDouble)
import Lib (readLines, split, splitOn)

-- (time, distance)
type Race = (Int, Int)

-- roots of equation (x)(t-x) = d
roots :: Race -> (Double, Double)
roots (t, d) =
  let t' = int2Double t
      d' = int2Double d
      s = sqrtDouble ((t' ** 2) - 4 * d')
   in ((t' - s) / 2, (t' + s) / 2)

-- returns least integer that is greater than input
ceilEx :: Double -> Int
ceilEx = (+ 1) . floorDouble

-- returns greatest integer that is less than input
floorEx :: Double -> Int
floorEx = (+ (-1)) . ceilingDouble

-- (a, b) ↦ { z ∈ ℤ | a < z < b }
integersBetween :: (Double, Double) -> [Int]
integersBetween (a, b) = [ceilEx a .. floorEx b]

readRaces :: [String] -> [Race]
readRaces [timeLine, distLine] =
  let f line = filter (not . null) $ filter (all isDigit) $ split (not . isDigit) $ last $ splitOn ':' line
      times = map read $ f timeLine
      dists = map read $ f distLine
   in zip times dists

day6a :: IO ()
day6a = do
  races <- readRaces <$> readLines
  print $ product $ map (length . integersBetween . roots) races

readRace :: [String] -> Race
readRace [timeLine, distLine] =
  let f line = concat $ filter (all isDigit) $ split (not . isDigit) $ last $ splitOn ':' line
      time = read $ f timeLine
      dist = read $ f distLine
   in (time, dist)

day6b :: IO ()
day6b = do
  race <- readRace <$> readLines
  print $ length $ integersBetween $ roots race
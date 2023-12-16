{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day5 (day5a, day5b) where

import Data.List (find, sort)
import Lib (chunksOf, readLines, split, splitOn)

-- start, len
type Range = (Int, Int)

inside :: Int -> Range -> Bool
inside x (start, len) = start <= x && x < start + len

-- (src range, dst range)
type Mapping = (Range, Range)

-- reads a single mapping from input
readMapping :: String -> Mapping
readMapping str = case map read $ words str of
  [dstStart, srcStart, len] -> ((srcStart, len), (dstStart, len))
  _ -> error "Invalid mapping"

src :: Mapping -> Range
src = fst

dst :: Mapping -> Range
dst = snd

type Map = [Mapping]

lookupMap :: Int -> Map -> Int
lookupMap x m = case find ((x `inside`) . src) m of
  Just ((srcStart, _), (dstStart, _)) -> dstStart + x - srcStart
  Nothing -> x

readMap :: [String] -> Map
readMap = map readMapping . tail

readSeeds :: String -> [Int]
readSeeds = map read . tail . splitOn ' '

seedToLocation :: [Map] -> Int -> Int
seedToLocation maps seed = foldl lookupMap seed maps

day5a :: IO ()
day5a = do
  inputLines <- readLines
  let groups = split null inputLines
  let seeds = readSeeds ((head . head) groups)
  let maps = map readMap (tail groups)
  print $ minimum (map (seedToLocation maps) seeds)

readSeedRanges :: String -> [Range]
readSeedRanges = map toPair . chunksOf 2 . readSeeds
  where
    toPair [x, y] = (x, y)
    toPair _ = error "Not a pair"

endpoints :: [Range] -> [Int]
endpoints = concatMap (\(s, l) -> [s, s + l])

partitionRange :: Range -> [Int] -> [Range]
partitionRange range@(s, l) cut =
  let cut' = filter (`inside` range) cut
      points = sort $ [s, s + l] ++ cut'
   in zipWith (curry mkRange) points (tail points)
  where
    mkRange (a, b) = (a, b - a)

intersect :: Range -> Range -> Range
intersect (s1, l1) (s2, l2) =
  let e1 = s1 + l1
      e2 = s2 + l2
      a = max s1 s2
      b = min e1 e2
   in (a, max 0 (b - a))

overlap :: Range -> Range -> Bool
overlap r1 r2 = snd (r1 `intersect` r2) /= 0

-- precondition: range intersects exactly at most one source range in m
transRange :: Map -> Range -> Range
transRange m range = case find (overlap range . src) m of
  Just mapping ->
    let inter = src mapping `intersect` range
        interLen = snd inter
     in (fst inter - (fst . src) mapping + (fst . dst) mapping, interLen)
  Nothing -> range

lookupRange :: Map -> Range -> [Range]
lookupRange m range =
  let mEndpoints = endpoints (map src m)
      -- partition along endpoints of source mappings in m to get precondition for transRange
      partRanges = partitionRange range mEndpoints
   in map (transRange m) partRanges

seedRangesToLocationRanges :: [Map] -> [Range] -> [Range]
seedRangesToLocationRanges maps ranges =
  foldl (\rs m -> concatMap (lookupRange m) rs) ranges maps

day5b :: IO ()
day5b = do
  inputLines <- readLines
  let groups = split null inputLines
  let seedRanges = readSeedRanges ((head . head) groups)
  let maps = map readMap (tail groups)
  print $ minimum $ map fst $ seedRangesToLocationRanges maps seedRanges
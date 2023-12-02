module Day1 (day1a, day1b) where

import Data.Bifunctor (bimap)
import Data.List (find, isPrefixOf)
import Lib (readLines)

isDigit :: Char -> Bool
isDigit ch = ch `elem` ['0' .. '9']

calibrationValue :: String -> Int
calibrationValue str =
  let digits = filter isDigit str
   in read (head digits : [last digits])

replaceLeft :: (Eq a) => [([a], [a])] -> [a] -> [a]
replaceLeft _ [] = []
replaceLeft reps xs =
  case find ((`isPrefixOf` xs) . fst) reps of
    Just (as, bs) -> bs ++ drop (length as) xs
    Nothing -> head xs : replaceLeft reps (tail xs)

replaceRight :: (Eq a) => [([a], [a])] -> [a] -> [a]
replaceRight reps xs = reverse $ replaceLeft (map (bimap reverse reverse) reps) (reverse xs)

digitReplacements :: [(String, String)]
digitReplacements =
  [ ("one", "1"),
    ("two", "2"),
    ("three", "3"),
    ("four", "4"),
    ("five", "5"),
    ("six", "6"),
    ("seven", "7"),
    ("eight", "8"),
    ("nine", "9")
  ]

calibrationValueReplDigits :: String -> Int
calibrationValueReplDigits str =
  let leftRepl = replaceLeft digitReplacements str
      leftDigits = filter isDigit leftRepl
      rightRepl = replaceRight digitReplacements str
      rightDigits = filter isDigit rightRepl
   in read (head leftDigits : [last rightDigits])

day1a :: IO ()
day1a = readLines >>= print . sum . map calibrationValue

day1b :: IO ()
day1b = readLines >>= print . sum . map calibrationValueReplDigits

module Lib
  ( readLines,
    split,
    strip,
  )
where

readLines :: IO [String]
readLines = lines <$> getContents

split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = [[]]
split on (x : xs)
  | x == on = [] : split on xs
  | otherwise =
      let rest = split on xs
       in (x : head rest) : tail rest

strip :: String -> String
strip = reverse . dropWhile isWhitespace . reverse . dropWhile isWhitespace
  where
    isWhitespace = (== ' ')
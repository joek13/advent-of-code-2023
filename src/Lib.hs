module Lib
  ( readLines,
    split,
    splitOn,
    strip,
  )
where

readLines :: IO [String]
readLines = lines <$> getContents

split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = [[]]
split on (x : xs)
  | on x = [] : split on xs
  | otherwise =
      let rest = split on xs
       in (x : head rest) : tail rest

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn x = split (== x)

strip :: String -> String
strip = reverse . dropWhile isWhitespace . reverse . dropWhile isWhitespace
  where
    isWhitespace = (== ' ')
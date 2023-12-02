module Lib
  ( readLines,
  )
where

readLines :: IO [String]
readLines = lines <$> getContents
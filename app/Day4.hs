module Day4 (day4a, day4b) where

import Lib (readLines, splitOn, strip)

-- (winning numbers, my numbers)
type Card = ([Int], [Int])

parseCard :: String -> Card
parseCard str =
  let cardStr = splitOn ':' str !! 1
      getNums = map (read . strip) . filter (not . null) . splitOn ' '
   in case splitOn '|' cardStr of
        [winningStr, myStr] -> (getNums winningStr, getNums myStr)
        _ -> error "Invalid card"

countWinners :: Card -> Int
countWinners (win, mine) = length $ filter (`elem` win) mine

score :: Card -> Int
score c = f (countWinners c)
  where
    f n
      | n == 0 = 0
      | n > 0 = 2 ^ (n - 1)
      | otherwise = error "Number of winners should be nonnegative"

day4a :: IO ()
day4a = do
  cards <- map parseCard <$> readLines
  print $ (sum . map score) cards

-- How many cards does a single instance of this card win us?
countCards ::
  Card ->
  -- | The card.
  [Int] ->
  -- | Scores of remaining cards.
  [Int]
-- \| Updated scores including this card.
countCards card nextScores =
  let winners = countWinners card
   in -- we get this card, plus however many the next w cards wins us.
      1 + sum (take winners nextScores) : nextScores

day4b :: IO ()
day4b = do
  cards <- map parseCard <$> readLines
  print $ sum $ foldr countCards [0] cards
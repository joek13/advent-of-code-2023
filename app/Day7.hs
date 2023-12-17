module Day7 (day7a, day7b) where

import Data.Function (on)
import Data.List (elemIndex, maximumBy, sort, sortBy)
import Data.Map (Map)
import Data.Map qualified as Map (assocs, elems, empty, insertWith)
import Data.Maybe (fromJust)
import Lib (readLines)

type Hand = String

counts :: (Ord a) => [a] -> Map a Int
counts = foldr f Map.empty
  where
    f a = Map.insertWith (+) a 1

handType :: Hand -> Int
handType hand =
  let cardCounts = sort $ Map.elems $ counts hand
   in case cardCounts of
        [5] -> 7 -- five of a kind
        [1, 4] -> 6 -- four of a kind
        [2, 3] -> 5 -- full house
        [1, 1, 3] -> 4 -- three of a kind
        [1, 2, 2] -> 3 -- two pair
        [1, 1, 1, 2] -> 2 -- one pair
        [1, 1, 1, 1, 1] -> 1 -- high card
        _ -> error $ "Unknown type for hand " <> hand

-- (part a) cards in order from weakest to strongest
cardsA :: String
cardsA = "23456789TJQKA"

orderRule :: String -> Hand -> Hand -> Ordering
orderRule _ [] [] = EQ
orderRule cards (a : as) (b : bs) =
  let aStrength = fromJust $ a `elemIndex` cards
      bStrength = fromJust $ b `elemIndex` cards
   in case compare aStrength bStrength of
        EQ -> orderRule cards as bs
        x -> x
orderRule _ _ _ = error "Invalid hand passed to orderRule"

compareHandA :: Hand -> Hand -> Ordering
compareHandA a b =
  let aType = handType a
      bType = handType b
   in case compare aType bType of
        EQ -> orderRule cardsA a b -- use order rule only if hands are of same type
        x -> x

readHandBid :: String -> (Hand, Int)
readHandBid line = case words line of
  [hand, bidStr] -> (hand, read bidStr)
  _ -> error "Invalid input"

solve :: (Hand -> Hand -> Ordering) -> IO ()
solve cmp = do
  handBids <- map readHandBid <$> readLines
  let compareHand' (h1, _) (h2, _) = cmp h1 h2
  let handBidsSorted = sortBy compareHand' handBids
  print $ sum $ zipWith (\r (_, b) -> r * b) [1 ..] handBidsSorted

day7a :: IO ()
day7a = solve compareHandA

-- replace jokers with most common non-joker card in hand
replJokers :: Hand -> Hand
replJokers hand
  | all (== 'J') hand = hand
  | otherwise =
      let otherCounts = counts (filter (/= 'J') hand)
          freqCard = maximumBy (compare `on` snd) (Map.assocs otherCounts)
          r 'J' = fst freqCard
          r x = x
       in map r hand

-- (part b) cards in order from weakest to strongest
cardsB :: String
cardsB = "J23456789TQKA"

compareHandB :: Hand -> Hand -> Ordering
compareHandB a b =
  let aType = handType (replJokers a) -- replace jokers for type comparison but not order comparison
      bType = handType (replJokers b)
   in case compare aType bType of
        EQ -> orderRule cardsB a b -- use order rule only if hands are of same type
        x -> x

day7b :: IO ()
day7b = solve compareHandB
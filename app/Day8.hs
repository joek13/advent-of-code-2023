module Day8 (day8a, day8b) where

import Data.Char (isAlphaNum)
import Data.Map (Map)
import Data.Map qualified as Map (fromList, keys, lookup)
import Data.Maybe (fromJust)
import Lib (readLines)

type Node = String

type Edges = (Node, Node)

readMapEntry :: String -> (Node, Edges)
readMapEntry str = case words str of
  [node, "=", left, right] ->
    let left' = filter isAlphaNum left
        right' = filter isAlphaNum right
     in (node, (left', right'))
  _ -> error "Invalid map entry"

type Direction = Char

type Path = [Direction]

select :: Direction -> Edges -> Node
select 'L' = fst
select 'R' = snd
select _ = error "Bad direction"

-- given a map, a path, and a start node,
-- returns list of nodes visited.
-- lazy with respect to input path
step :: Map Node Edges -> Path -> Node -> [Node]
step _ [] cur = [cur]
step m (d : ds) cur = cur : step m ds next
  where
    next = select d $ fromJust $ cur `Map.lookup` m

day8a :: IO ()
day8a = do
  inputLines <- readLines
  let path = cycle (head inputLines)
  let m = Map.fromList $ map readMapEntry $ drop 2 inputLines
  print $ length $ takeWhile (/= "ZZZ") $ step m path "AAA"

isInitial :: Node -> Bool
isInitial = (== 'A') . last

isTerminal :: Node -> Bool
isTerminal = (== 'Z') . last

day8b :: IO ()
day8b = do
  inputLines <- readLines
  let path = cycle (head inputLines)
  let m = Map.fromList $ map readMapEntry $ drop 2 inputLines
  let initialStates = filter isInitial $ Map.keys m
  -- order of cycle given by each initial state
  let orders = map (length . takeWhile (not . isTerminal) . step m path) initialStates
  -- compute lcm
  print $ foldl lcm 1 orders
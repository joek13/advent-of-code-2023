module Day3 (day3a, day3b) where

import Lib (readLines, split)

isDigit :: Char -> Bool
isDigit ch = ch `elem` ['0' .. '9']

type Posn = (Int, Int)

data Item = Item {posn :: Posn, width :: Int, what :: What}
  deriving (Eq, Show)

isSymb :: Item -> Bool
isSymb it = case what it of
  (Symbol _) -> True
  _ -> False

isNum :: Item -> Bool
isNum it = case what it of
  (Number _) -> True
  _ -> False

getNum :: Item -> Int
getNum it = case what it of
  (Number x) -> x
  _ -> error "Not a Number"

data What = Symbol Char | Number Int
  deriving (Eq, Show)

type World = (Int, Int, [Item])

items :: World -> [Item]
items (_, _, its) = its

findNumbers :: String -> [(Int, String)]
findNumbers =
  map (\xs -> ((fst . head) xs, map snd xs))
    . filter (not . null)
    . split (not . isDigit . snd)
    . zip [0 :: Int ..]

findSymbols :: String -> [(Int, Char)]
findSymbols = filter (f . snd) . zip [0 ..]
  where
    f ch = (not . isDigit) ch && ch /= '.'

mkNumber :: Int -> (Int, String) -> Item
mkNumber r (c, str) = Item (r, c) (length str) (Number (read str))

mkSymb :: Int -> (Int, Char) -> Item
mkSymb r (c, ch) = Item (r, c) 1 (Symbol ch)

parseRow :: Int -> String -> [Item]
parseRow r row =
  let nums = findNumbers row
      symbs = findSymbols row
   in map (mkNumber r) nums ++ map (mkSymb r) symbs

parseWorld :: [String] -> World
parseWorld inLines =
  let width = length (head inLines)
      height = length inLines
      its = concat $ zipWith parseRow [0 ..] inLines
   in (height, width, its)

contains :: Item -> Posn -> Bool
contains it (i, j) =
  let (r, c) = posn it
      w = width it
   in r == i && c <= j && j < c + w

neighborPosns :: Posn -> Int -> [Posn]
neighborPosns (i, j) w =
  [(i - 1, k) | k <- [j - 1 .. j + w]] -- row above
    ++ [(i + 1, k) | k <- [j - 1 .. j + w]] -- row below
    ++ [(i, j - 1), (i, j + w)] -- sides

neighbors :: World -> Item -> [Item]
neighbors world it =
  let (r, c) = posn it
      w = width it
      adj = neighborPosns (r, c) w
   in filter (\ot -> any (ot `contains`) adj) (items world)

day3a :: IO ()
day3a = do
  world <- parseWorld <$> readLines
  let neighbors' = neighbors world
  print $ (sum . map getNum . filter (any isSymb . neighbors') . filter isNum . items) world

isGear :: World -> Item -> Bool
isGear world it = case what it of
  Symbol '*' -> (length . filter isNum) (neighbors world it) >= 2
  _ -> False

gearRatio :: World -> Item -> Int
gearRatio world = product . map getNum . filter isNum . neighbors world

day3b :: IO ()
day3b = do
  world <- parseWorld <$> readLines
  let gearRatio' = gearRatio world
  let isGear' = isGear world
  print $ (sum . map gearRatio' . filter isGear' . items) world
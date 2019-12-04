{-# LANGUAGE OverloadedStrings #-}
module Day3 where

import           Data.List  (sortOn)
import           Data.Maybe (catMaybes)

import qualified Data.Text  as T

day3_1 :: IO Int
day3_1 = do
    input <- lines <$> readFile "/Users/toku/Haskell/Advent-of-code-2019/src/day3.txt"
    let [wire1,wire2] = toNumbers . T.splitOn "," . T.pack <$> input
    let lines1 = zip wire1 (tail wire1)
    let lines2 = zip wire2 (tail wire2)
    let intersections = sortOn manhattanDistance $ concat $ [ catMaybes $ fmap (compareWires line1) lines2 | line1 <- lines1 ]
    pure $ manhattanDistance $ head intersections
  where
    manhattanDistance (x,y) = abs x + abs y
    compareWires ((x11,y11),(x12,y12)) ((x21,y21),(x22,y22))
        | x11 /= x12 && x21 == x22 && (min x11 x12) < x21 && (max x11 x12) > x21 && y11 > (min y21 y22) && y11 < (max y21 y22) = Just (x21,y12)
        | x11 == x12 && x21 /= x22 && (min x21 x22) < x11 && (max x21 x22) > x11 && y21 > (min y11 y12) && y21 < (max y11 y12) = Just (x11,y21)
        | otherwise = Nothing
    toNumbers = foldl toNextCoord [(0,0)]
    toNextCoord res move =
        case (T.splitAt 1 move, last res) of
          (("R", num), (x,y)) -> res <> [(x + read (T.unpack num), y)]
          (("L", num), (x,y)) -> res <> [(x - read (T.unpack num), y)]
          (("U", num), (x,y)) -> res <> [(x, y + read (T.unpack num))]
          (("D", num), (x,y)) -> res <> [(x, y - read (T.unpack num))]

day3_2 :: IO Int
day3_2 = do
    input <- lines <$> readFile "/Users/toku/Haskell/Advent-of-code-2019/src/day3.txt"
    let [wire1,wire2] = toNumbers . T.splitOn "," . T.pack <$> input
    let lines1 = zip wire1 (tail wire1)
    let lines2 = zip wire2 (tail wire2)
    let steps = sortOn (stepCount lines1 lines2) $ concat $ [ catMaybes $ fmap (compareWires line1) lines2 | line1 <- lines1 ]
    pure $ stepCount lines1 lines2 $ head steps
  where
    manhattanDistance ((x1,y1),(x2,y2)) = abs (x1 - x2) + abs (y1 - y2)
    notInLine (x,y) ((x1,y1), (x2,y2)) | x1 == x2 && y > (min y1 y2) && y < (max y1 y2) && x1 == x = False
                                       | y1 == y2 && x > (min x1 x2) && x < (max x1 x2) && y1 == y = False
                                       | otherwise = True
    stepCount wire1 wire2 (x,y) = stepCountForWire wire1 (x,y) + stepCountForWire wire2 (x,y)
    stepCountForWire wire (x,y) = foldr (\((x1,y1),(x2,y2)) sum -> if notInLine (x,y) ((x1,y1),(x2,y2)) then
                                                                     sum + manhattanDistance ((x1,y1),(x2,y2))
                                                                   else
                                                                     manhattanDistance ((x1,y1),(x,y))) 0 wire
    compareWires ((x11,y11),(x12,y12)) ((x21,y21),(x22,y22))
        | x11 /= x12 && x21 == x22 && (min x11 x12) < x21 && (max x11 x12) > x21 && y11 > (min y21 y22) && y11 < (max y21 y22) = Just (x21,y12)
        | x11 == x12 && x21 /= x22 && (min x21 x22) < x11 && (max x21 x22) > x11 && y21 > (min y11 y12) && y21 < (max y11 y12) = Just (x11,y21)
        | otherwise = Nothing
    toNumbers = foldl toNextCoord [(0,0)]
    toNextCoord res move =
        case (T.splitAt 1 move, last res) of
          (("R", num), (x,y)) -> res <> [(x + read (T.unpack num), y)]
          (("L", num), (x,y)) -> res <> [(x - read (T.unpack num), y)]
          (("U", num), (x,y)) -> res <> [(x, y + read (T.unpack num))]
          (("D", num), (x,y)) -> res <> [(x, y - read (T.unpack num))]

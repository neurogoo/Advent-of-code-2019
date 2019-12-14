module Day8 where

import           Data.List (sortOn)

day8_1 :: IO Int
day8_1 = do
    let width = 25
    let height = 6
    input <- readFile "/Users/toku/Haskell/Advent-of-code-2019/src/day8.txt"
    let numbers = fmap ((read :: String -> Int) . pure) $ head $ lines input
    let layers = toLayers [] (width * height) numbers
    let minimumLayer = head $ sortOn (numberOfDigits 0) layers
    pure $ (numberOfDigits 1 minimumLayer) * (numberOfDigits 2 minimumLayer)
  where
    toLayers layers size [] = layers
    toLayers layers size xs = toLayers (layers <> [take size xs]) size (drop size xs)

    numberOfDigits digit layer = length $ filter (== digit) layer

day8_2 :: IO ()
day8_2 = do
    let width = 25
    let height = 6
    input <- readFile "/Users/toku/Haskell/Advent-of-code-2019/src/day8.txt"
    let numbers = fmap ((read :: String -> Int) . pure) $ head $ lines input
    let layers = toLayers [] (width * height) numbers
    printPicture width $ foldl combineLayers (head layers) (tail layers)
    pure ()
  where
    toLayers layers size [] = layers
    toLayers layers size xs = toLayers (layers <> [take size xs]) size (drop size xs)

    combinePixels (first,second) =
        if first == 2 then
          second
        else
          first

    combineLayers :: [Int] -> [Int] -> [Int]
    combineLayers res layer = map combinePixels $ zip res layer

    printPicture size [] = pure ()
    printPicture size xs = do
        print $ concat $ map (\x -> if x == 0 then "#" else " ") (take size xs)
        printPicture size (drop size xs)

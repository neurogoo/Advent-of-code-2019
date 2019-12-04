module Day1 where

day1_1 :: IO Int
day1_1 = do
    input <- lines <$> readFile "/Users/toku/Haskell/Advent-of-code-2019/day1.txt"
    let numbers = (read :: String -> Int) <$> input
    pure $ sum $ (\m -> m `div` 3 - 2) <$> numbers

day1_2 :: IO Int
day1_2 = do
    input <- lines <$> readFile "/Users/toku/Haskell/Advent-of-code-2019/day1.txt"
    let numbers = (read :: String -> Int) <$> input
    let go sum fuel | fuel > 0 = let newfuel = fuel `div` 3 - 2
                                 in go (sum + fuel) newfuel
        go sum _ = sum
    pure $ sum $ (\m -> go 0 (m `div` 3 - 2)) <$> numbers

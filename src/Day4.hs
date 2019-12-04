module Day4 where

import           Data.List

day4_1 :: Int
day4_1 = let lowerBound = undefined
             upperBound = undefined
             isIncreasing num = show num == sort (show num)
             hasOneDuplicate num = (length $ filter (\(a,b) -> a == b) $ zip (show num) (tail $ show num)) > 0
         in length $ filter isIncreasing $ filter hasOneDuplicate $ [lowerBound .. upperBound]

day4_2 :: Int
day4_2 = let lowerBound = undefined
             upperBound = undefined
             isIncreasing num = show num == sort (show num)
             hasOneDuplicate num = (length $ filter (\x -> length x == 2) $ group $ show num) > 0
         in length $ filter isIncreasing $ filter hasOneDuplicate $ [lowerBound .. upperBound]

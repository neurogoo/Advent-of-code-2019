{-# LANGUAGE OverloadedStrings #-}
module Day2 where

import qualified Data.Sequence as S
import qualified Data.Text     as T

day2_1 :: IO Int
day2_1 = do
    input <- T.pack <$> readFile "/Users/toku/Haskell/Advent-of-code-2019/src/day2.txt"
    let numbers = S.fromList $ ((read :: String -> Int) . T.unpack) <$> T.splitOn "," input
    pure $ flip S.index 0 $ runProgram (S.update 1 12 $ S.update 2 2 numbers) 0
  where
    runProgram arr index =
        case arr `S.index` index of
          99 -> arr
          1  -> let firstPos = arr `S.index` (index + 1)
                    secondPos = arr `S.index` (index + 2)
                    resPos = arr `S.index` (index + 3)
                in runProgram (S.update resPos (arr `S.index` firstPos + arr `S.index` secondPos) arr) (index + 4)
          2 ->  let firstPos = arr `S.index` (index + 1)
                    secondPos = arr `S.index` (index + 2)
                    resPos = arr `S.index` (index + 3)
                in runProgram (S.update resPos (arr `S.index` firstPos * arr `S.index` secondPos) arr) (index + 4)

day2_2 :: IO (Int,Int,Int)
day2_2 = do
    input <- T.pack <$> readFile "/Users/toku/Haskell/Advent-of-code-2019/src/day2.txt"
    let numbers = S.fromList $ ((read :: String -> Int) . T.unpack) <$> T.splitOn "," input
    pure $ head $ filter (\(val,_,_) -> val == 19690720) [(flip S.index 0 $ runProgram (S.update 1 noun $ S.update 2 verb numbers) 0, noun, verb) | noun <- [0..99], verb <- [0..99]]
  where
    runProgram arr index =
        case arr `S.index` index of
          99 -> arr
          1  -> let firstPos = arr `S.index` (index + 1)
                    secondPos = arr `S.index` (index + 2)
                    resPos = arr `S.index` (index + 3)
                in runProgram (S.update resPos (arr `S.index` firstPos + arr `S.index` secondPos) arr) (index + 4)
          2 ->  let firstPos = arr `S.index` (index + 1)
                    secondPos = arr `S.index` (index + 2)
                    resPos = arr `S.index` (index + 3)
                in runProgram (S.update resPos (arr `S.index` firstPos * arr `S.index` secondPos) arr) (index + 4)

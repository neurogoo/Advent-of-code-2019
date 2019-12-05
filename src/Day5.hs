{-# LANGUAGE OverloadedStrings #-}
module Day5 where

import           Debug.Trace

import qualified Data.Sequence as S
import qualified Data.Text     as T

runProgram :: S.Seq Int -> Int -> Int -> (S.Seq Int,Int)
runProgram arr index input =
    let instructionCode = arr `S.index` index
        (ins,modes) = splitAt 2 $ reverse $ show instructionCode
        ins' = (read :: String -> Int) $ reverse $ ins
        modes' = map ((read :: String -> Int) . pure) modes <> (repeat 0)
    in case ins' of
         99 -> (arr, input)
         1  -> let firstPos = getValue arr (index + 1) (modes' !! 0)
                   secondPos = getValue arr (index + 2) (modes' !! 1)
                   resPos = arr `S.index` (index + 3)
               in runProgram (S.update resPos (firstPos + secondPos) arr) (index + 4) 0
         2 ->  let firstPos = getValue arr (index + 1) (modes' !! 0)
                   secondPos = getValue arr (index + 2) (modes' !! 1)
                   resPos = arr `S.index` (index + 3)
               in runProgram (S.update resPos (firstPos * secondPos) arr) (index + 4) 0
         3 -> let savePos = arr `S.index` (index + 1)
              in runProgram (S.update savePos input arr) (index + 2) 0
         4 -> let valPos = getValue arr (index + 1) (modes' !! 0)
              in runProgram arr (index + 2) valPos
         5 -> let firstPos = getValue arr (index + 1) (modes' !! 0)
                  secondPos = getValue arr (index + 2) (modes' !! 1)
              in if firstPos /= 0 then
                   runProgram arr secondPos 0
                 else
                   runProgram arr (index + 3) 0
         6 -> let firstPos = getValue arr (index + 1) (modes' !! 0)
                  secondPos = getValue arr (index + 2) (modes' !! 1)
              in if firstPos == 0 then
                   runProgram arr secondPos 0
                 else
                   runProgram arr (index + 3) 0
         7 -> let firstPos = getValue arr (index + 1) (modes' !! 0)
                  secondPos = getValue arr (index + 2) (modes' !! 1)
                  resPos = arr `S.index` (index + 3)
              in if firstPos < secondPos then
                   runProgram (S.update resPos 1 arr) (index + 4) 0
                 else
                   runProgram (S.update resPos 0 arr) (index + 4) 0
         8 -> let firstPos = getValue arr (index + 1) (modes' !! 0)
                  secondPos = getValue arr (index + 2) (modes' !! 1)
                  resPos = arr `S.index` (index + 3)
              in if firstPos == secondPos then
                   runProgram (S.update resPos 1 arr) (index + 4) 0
                 else
                   runProgram (S.update resPos 0 arr) (index + 4) 0
  where
    getValue arr pos 1 = arr `S.index` pos
    getValue arr pos _ = arr `S.index` (arr `S.index` pos)

day5_1 :: IO Int
day5_1 = do
    input <- T.pack <$> readFile "/Users/toku/Haskell/Advent-of-code-2019/src/day5.txt"
    let numbers = S.fromList $ ((read :: String -> Int) . T.unpack) <$> T.splitOn "," input
    pure $ snd $ runProgram numbers 0 1

day5_2 :: IO Int
day5_2 = do
    input <- T.pack <$> readFile "/Users/toku/Haskell/Advent-of-code-2019/src/day5.txt"
    let numbers = S.fromList $ ((read :: String -> Int) . T.unpack) <$> T.splitOn "," input
    pure $ snd $ runProgram numbers 0 5

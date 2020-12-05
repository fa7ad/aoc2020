{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TI
import Data.Text.Read

main :: IO ()
main = do
  slopes <- map (map parseInt . T.splitOn " ") . T.lines <$> TI.readFile "slopes.txt"
  lns <- T.lines <$> TI.readFile "input.txt"
  let total = foldl (\a [h, v] -> (a *) $ snd $ foldl (countTrees h) (0, 0) $ filterIndexed (isIdxDivBy v) lns) 1 slopes
  print total

isIdxDivBy :: Integral a1 => a1 -> (a2, a1) -> Bool
isIdxDivBy ver = (0 ==) . (`mod` ver) . snd

parseInt :: Integral p => T.Text -> p
parseInt x = case decimal x of
  Right (v, _) -> v
  _ -> 0

countTrees :: Num b => Int -> (Int, b) -> T.Text -> (Int, b)
countTrees h (mul, cnt) line = case T.index line ((h * mul) `mod` T.length line) of
  '#' -> (mul + 1, cnt + 1)
  _ -> (mul + 1, cnt)

filterIndexed :: ((a, Int) -> Bool) -> [a] -> [a]
filterIndexed p xs = [x | (x, i) <- zip xs [0 .. (length xs)], p (x, i)]

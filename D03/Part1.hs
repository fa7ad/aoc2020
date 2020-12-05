{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TI

main :: IO ()
main = do
  s <- TI.readFile "input.txt"
  let (_, count) = foldl countTrees (0, 0) $ T.lines s
  print count

countTrees :: (Int, Int) -> T.Text -> (Int, Int)
countTrees (mul, cnt) line = case T.index line ((3 * mul) `mod` T.length line) of
  '#' -> (mul + 1, cnt + 1)
  _ -> (mul + 1, cnt)

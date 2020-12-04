{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T

main :: IO ()
main = do
  s <- readFile "input.txt"
  let matched = filter (matchesRule . toRuleInputPair) $ T.lines $ T.pack s
  print (length matched)

toRuleInputPair :: T.Text -> (T.Text, T.Text)
toRuleInputPair line = (rule, input)
  where
    [rule, input] = T.splitOn ": " line

matchesRule :: (T.Text, T.Text) -> Bool
matchesRule (rule, input) = result
  where
    [r, c] = T.splitOn " " rule
    [l, h] = map (T.index input . subtract 1 . (read :: String -> Int) . T.unpack) $ T.splitOn "-" r
    result = (l == T.head c) /= (h == T.head c)
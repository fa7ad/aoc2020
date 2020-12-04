{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Data.Text.Read

main :: IO ()
main = do
  s <- readFile "input.txt"
  let matched = filter (matchesRule . toRuleInputPair) $ T.lines $ T.pack s
  print (length matched)

parseInt :: Integral p => T.Text -> p
parseInt s = case decimal s of
  Right (v, _) -> v
  Left _ -> 0

toRuleInputPair :: T.Text -> (T.Text, T.Text)
toRuleInputPair line = (rule, input)
  where
    [rule, input] = T.splitOn ": " line

matchesRule :: (T.Text, T.Text) -> Bool
matchesRule (rule, input) = result
  where
    [r, c] = T.splitOn " " rule
    [l, h] = map parseInt $ T.splitOn "-" r
    cnt = T.count c input
    result = l <= cnt && cnt <= h
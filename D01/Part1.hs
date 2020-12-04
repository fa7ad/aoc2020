module Main where

import Data.List

main :: IO ()
main = do
  s <- readFile "input.txt"
  let nums = sort $ map (read :: String -> Int) $ lines s
  case twoSum 2020 nums of
    Just (x, y) -> print (x * y)
    Nothing -> print "No solutions"

twoSum :: (Integral t) => t -> [t] -> Maybe (t, t)
twoSum _ [] = Nothing
twoSum t (x : xs) = case find (== (t - x)) xs of
  Just y -> Just (x, y)
  Nothing -> twoSum t xs
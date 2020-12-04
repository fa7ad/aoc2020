module Main where

import Data.List

main :: IO ()
main = do
  s <- readFile "input.txt"
  let nums = sort $ map (read :: String -> Int) $ lines s
  case threeSum 2020 nums of
    Just (x, y, z) -> print (x * y * z)
    Nothing -> print "No solutions"

twoSum :: (Integral t) => t -> [t] -> Maybe (t, t)
twoSum _ [] = Nothing
twoSum t (x : xs) = case find (== (t - x)) xs of
  Just y -> Just (x, y)
  Nothing -> twoSum t xs

threeSum :: (Integral t) => t -> [t] -> Maybe (t, t, t)
threeSum _ [] = Nothing
threeSum t (x : xs) = case twoSum (t - x) xs of
  Just (y, z) -> Just (x, y, z)
  Nothing -> threeSum t xs
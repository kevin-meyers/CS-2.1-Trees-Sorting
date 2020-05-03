module Main where

import Lib
import Utils

main :: IO ()
main = putStrLn "Please"

-- should let ys be anything equatable
data DecisionTree a
  = Leaf Int
  | Node ([a] -> Bool) (DecisionTree a) (DecisionTree a)

predict :: (Num a) => [a] -> DecisionTree a -> Int
predict _ (Leaf x) = x
predict xs (Node thresh l r)
  | thresh xs = predict xs r
  | otherwise = predict xs l

train :: [[Double]] -> [Int] -> DecisionTree Double
train xs ys = buildTree $ partitionsFrom xs ys

buildTree :: Inputs Double -> DecisionTree Double
buildTree (xs, ys) = Leaf 5
  where
    (bestFeat, bestIndex) = getBestSplit ys
    (left, right) = childrenData bestIndex ys
    t = midPoint bestIndex (xs !! bestFeat)

midPoint :: Int -> [Double] -> Double
midPoint i ys = (ys !! i + (ys !! (i - 1))) / 2

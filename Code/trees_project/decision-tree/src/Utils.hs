{-# LANGUAGE TupleSections #-}

module Utils where

import Data.Function (on)
import Data.List (sortBy)
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet

type PartitionPair = (MultiSet Int, MultiSet Int)

orderedYs :: (Ord a) => [a] -> [Int] -> ([a], [Int])
orderedYs xs ys =
  foldr (\(x, y) (xs', ys') -> (x : xs', y : ys')) ([], []) (sortTuple xs ys)

sortTuple :: (Ord a) => [a] -> [b] -> [(a, b)]
sortTuple xs ys = sortBy (compare `on` fst) (zip xs ys)

partition :: [Int] -> [PartitionPair]
partition xs = scanr shiftMaps (MultiSet.fromList xs, MultiSet.empty) xs

shiftMaps :: Int -> PartitionPair -> PartitionPair
shiftMaps x (left, right) = (MultiSet.delete x left, MultiSet.insert x right)

partitionsFrom :: (Ord a) => [[a]] -> [Int] -> [[PartitionPair]]
partitionsFrom xs ys = map (`createFeatures` ys) xs

createFeatures :: (Ord a) => [a] -> [Int] -> [PartitionPair]
createFeatures xs ys = partition inOrderYs
  where
    (thresh, inOrderYs) = orderedYs xs ys

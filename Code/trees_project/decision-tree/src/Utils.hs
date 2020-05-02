{-# LANGUAGE TupleSections #-}

module Utils
  (
  ) where

import Data.Function (on)
import Data.List (sortBy)
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet

orderedYs :: (Ord a) => [a] -> [Int] -> ([a], [Int])
orderedYs xs ys =
  foldr (\(x, y) (xs', ys') -> (x : xs', y : ys')) ([], []) (sortTuple xs ys)

sortTuple :: (Ord a) => [a] -> [b] -> [(a, b)]
sortTuple xs ys = sortBy (compare `on` fst) (zip xs ys)

partitionsFrom :: [Int] -> [(MultiSet Int, MultiSet Int)]
partitionsFrom xs = scanr shiftMaps (MultiSet.empty, MultiSet.fromList xs) xs

shiftMaps :: Int -> (MultiSet Int, MultiSet Int) -> (MultiSet Int, MultiSet Int)
shiftMaps x (left, right) = (MultiSet.insert x left, MultiSet.delete x right)

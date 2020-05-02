module Lib
  (
  ) where

import Control.Lens
import qualified Data.MultiSet as M

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Probabilities = [Double]

type Offset = Int

type MinSplitter = (Double, Offset) -- gini_index, offset

type Partition = [Int]

type EnumPartition = (Offset, Partition, Partition)

type Partitions = [EnumPartition]

type Features = [(Int, Partitions)]

probabilitiesFromP :: Partition -> Probabilities
probabilitiesFromP xs = map ((/ len) . fromIntegral) xs
  where
    len = fromIntegral $ sum xs

probabilitiesFor :: [Int] -> Probabilities
probabilitiesFor xs =
  M.foldOccur (\_ freq acc -> (fromIntegral freq / x_size) : acc) [] hist
  where
    hist = M.fromList xs
    x_size = fromIntegral $ length xs

entropyOf :: Probabilities -> Double
entropyOf = sum . map (\x -> -x * log x)

giniIndexOf :: Probabilities -> Double
giniIndexOf = (1 -) . sum . map (^ 2)

updateAcc :: MinSplitter -> MinSplitter -> MinSplitter
updateAcc acc@(min_gini, _) comp@(comp_gini, _)
  | comp_gini < min_gini = comp
  | otherwise = acc

totoalGini :: Int -> EnumPartition -> Double
totoalGini len (offset, leftCounts, rightCounts) =
  weightedGini offset leftCounts + weightedGini (len - offset) rightCounts

minGini :: Partitions -> MinSplitter
minGini xs@((_, _, base):_) =
  foldr (\p acc -> updateAcc acc (totoalGini len p, view _1 p)) (baseGini, 0) xs
  where
    len = length xs
    baseGini = giniIndexOf $ probabilitiesFromP base

weightedGini :: Int -> Partition -> Double
weightedGini offset = (fromIntegral offset *) . giniIndexOf . probabilitiesFromP

bestSplit :: Features -> (Int, Int)
bestSplit =
  dec . foldr (\(i, x) acc -> isBetterGini (minGini x) i acc) (2.0, 0, 0) -- gini can at most be 1

dec :: (Int, Int, Int) -> (Int, Int)
dec (_, x, y) = (x, y)

isBetterGini :: MinSplitter -> Int -> (Double, Int, Int) -> (Double, Int, Int)
isBetterGini (gini, offset) feat acc@(accGini, _, _)
  | gini < accGini = (gini, feat, offset)
  | otherwise = acc
-- Creating the partitions! For each "feature"/column from X we make a tuple
-- list of the feature tied to y. Then we sort the list based on the feature
--  then going down the list one at a time, building partitions and the index
--  so like if the data looks like [1, 2, 1, 1, 2, 3, 1, 2]
-- i need to start with 1 [1, 0, 0] [3, 3, 1], then 2 [1, 1, 0] [3, 2, 1]
-- consider using a map of the unique values of y, so i can label encode
-- OR NEW IDEA, do the data preprocessing in python or something, and pass
-- only numerical features and 0 offset targets.

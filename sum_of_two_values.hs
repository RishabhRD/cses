-- pragmas.hs {{{
-- vim: foldmethod=marker
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE BinaryLiterals             #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE UndecidableInstances       #-}
-- pragmas.hs }}}
module Main where

import           Control.Monad         (replicateM_)
import           Data.Array            (Array, (!))
import qualified Data.Array            as Array
import           Data.Bifunctor        (Bifunctor (bimap))
import qualified Data.ByteString.Char8 as C
import           Data.Ix               (Ix)
import           Data.List             (find, sortOn, unfoldr)
import           Data.Maybe            (listToMaybe)
import           Debug.Trace           (trace)

solve' :: Int -> Int -> [Int] -> Maybe (Int, Int)
solve' n k xs =  find isValid (zip indexes (toOther <$> indexes))
  where
  arr = Array.listArray (0, n- 1) xs
  indexes = [0..n -1]
  toOther idx = lowerBound (idx + 1) n (k - (arr ! idx)) arr
  isValid (a, b) = (b /= n) && (arr ! a + arr ! b == k)

solve :: (Num a, Enum a) => Int -> Int -> [Int] -> Maybe (a, a)
solve n k xs = fmap (bimap origIndex origIndex) $ solve' n k $ snd <$> sortedArray
  where
  sortedArray = sortOn snd $ zip [1..] xs
  origIndex i = fst $ sortedArray !! i

class (Ix a) => Indexable f a b where
  atIdx :: f a b -> a -> b

instance Ix a => Indexable Array a b where
  atIdx :: Ix a => Array a b -> a -> b
  atIdx = (!)

partitionPoint :: (Enum a, Indexable f a b) => a -> a -> (b -> Bool) -> f a b -> a
-- range: [low, high)
-- returns index of first element that doesn't satisfy criteria
partitionPoint low' high' isCandidate range = bs low' high'
  where
  bs low high
    | low == high = low
    | isCandidate (atIdx range mid) = bs (next mid) high
    | otherwise = bs low mid
    where mid = toEnum $ (fromEnum low + fromEnum high) `div` 2
  next x = toEnum $ fromEnum x + 1

lowerBound :: (Enum a, Indexable f a b, Ord b) => a -> a -> b -> f a b -> a
lowerBound low high n = partitionPoint low high (<n)

upperBound :: (Enum a, Indexable f a b, Ord b) => a -> a -> b -> f a b -> a
upperBound low high n = partitionPoint low high (<=n)

readInt :: C.ByteString -> Int
readInt s = let Just (i,_) = C.readInt s in i :: Int

readInt2 :: C.ByteString -> (Int, Int)
readInt2 u = (a, b)
  where
  Just (a,v) = C.readInt u
  Just (b,_) = C.readInt (C.tail v)

readInts :: C.ByteString -> [Int]
readInts = unfoldr go where
    go s = do
        (n,s1) <- C.readInt s
        let s2 = C.dropWhile (==' ') s1
        pure (n,s2)

getInt :: IO Int
getInt  = readInt <$> C.getLine

getInt2 :: IO (Int, Int)
getInt2 = readInt2 <$> C.getLine

getInts :: IO [Int]
getInts = readInts <$> C.getLine

showMaybe :: Maybe (Int, Int) -> String
showMaybe Nothing       = "IMPOSSIBLE"
showMaybe (Just (a, b)) = show a ++ " " ++ show b

main :: IO ()
main = do
  (n, k) <- getInt2
  xs <- getInts
  putStrLn $ showMaybe $ solve  n k xs

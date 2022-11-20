-- pragmas.hs  {{{
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
{-# LANGUAGE InstanceSigs               #-}
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

import           Control.Monad            (replicateM_)
import           Data.Array               (Array, (!))
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as C
import qualified Data.ByteString.Internal as BSI
import qualified Data.IntMap              as IntMap
import           Data.Ix                  (Ix)
import           Debug.Trace              (trace)

solve :: [Int] -> Int -> Int
solve nums k = partitionPoint low high can_not_be_maximum $ Function id
  where
  low = maximum nums
  high = maxBound :: Int
  can_not_be_maximum e = fst (foldl op (1, 0) nums) > k
    where op (cnt, sum) ele
            | sum + ele > e = (1 + cnt, ele)
            | otherwise = (cnt, sum + ele)

class Indexable f a where
  atIdx :: f a -> Int -> a

instance Indexable (Array Int) b where
  atIdx :: Array Int a -> Int -> a
  atIdx = (!)

newtype Function a b = Function (a -> b)

instance Indexable (Function Int) b where
  atIdx :: Function Int b -> Int -> b
  atIdx (Function f) = f

instance Indexable IntMap.IntMap a where
  atIdx :: IntMap.IntMap a -> Int -> a
  atIdx = (IntMap.!)

partitionPoint :: (Indexable f b) => Int -> Int -> (b -> Bool) -> f b -> Int
-- range: [low, high)
-- returns index of first element that doesn't satisfy predicate
partitionPoint low' high' isCandidate range = bs low' high'
  where
  bs low high
    | low == high = low
    | isCandidate (atIdx range mid) = bs (mid + 1) high
    | otherwise = bs low mid
    where mid = low + ((high - low) `div` 2)

lowerBound :: (Indexable f b, Ord b) => Int -> Int -> b -> f b -> Int
lowerBound low high n = partitionPoint low high (<n)

upperBound :: (Indexable f b, Ord b) => Int -> Int -> b -> f b -> Int
upperBound low high n = partitionPoint low high (<=n)

main :: IO ()
main = do
  (_, k) <- getInt2
  xs <- getInts
  print $ solve xs k

readInt :: C.ByteString -> Int
readInt s = let Just (i,_) = C.readInt s in i :: Int

readInt2 :: C.ByteString -> (Int, Int)
readInt2 u = (a, b)
  where
  Just (a,v) = C.readInt u
  Just (b,_) = C.readInt (C.tail v)

readInts :: C.ByteString -> [Int]
readInts str = readInt <$> BS.split (BSI.c2w ' ') str

getInt :: IO Int
getInt  = readInt <$> C.getLine

getInt2 :: IO (Int, Int)
getInt2 = readInt2 <$> C.getLine

getInts :: IO [Int]
getInts = readInts <$> C.getLine

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

import           Control.Monad            (replicateM, replicateM_)
import           Data.Array               (Array, (!))
import           Data.Bool                (bool)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as C
import qualified Data.ByteString.Internal as BSI
import qualified Data.IntMap              as IntMap
import           Data.Ix                  (Ix)
import           Data.List
import           Debug.Trace              (trace)

-- sorted list
solve' :: [Int] -> [Bool]
solve' xs = zipWith (<=) xs maxes
  where
  maxes = scanl' max (-1) xs

solve'' :: [Int] -> [Bool]
solve'' xs = reverse $ zipWith (>=) rxs mins
  where
  rxs = reverse xs
  mins = scanl' min inf rxs

inf :: Int
inf = floor (1e9 + 10)

indexList :: [Int]
indexList = [1..]

sortByIndex :: [Int] -> [a] -> [a]
sortByIndex indexes lst = fmap snd $ sortOn fst $ zip indexes lst

solve :: [(Int, Int)] -> ([Bool], [Bool])
solve xs = (ans, ans')
  where
  sorted = sortBy cmp $ zip indexList xs
  indexes = fmap fst sorted
  ends = fmap (snd . snd) sorted
  ans = sortByIndex indexes $ solve'' ends
  ans' = sortByIndex indexes $ solve' ends
  getLow = fst . snd
  getHigh = snd . snd
  cmp a b
    | getLow a == getLow b = compare (getHigh b) (getHigh a)
    | otherwise = compare (getLow a) (getLow b)

toString :: [Bool] -> String
toString = unwords . fmap (bool "0" "1")

main :: IO ()
main = do
  n <- getInt
  xs <- replicateM n getInt2
  let (ans, ans') = solve xs
  putStrLn $ toString ans
  putStrLn $ toString ans'

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

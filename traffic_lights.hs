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
import qualified Data.Map                 as Map
import           Data.Maybe               (fromJust)
import qualified Data.Set                 as Set
import           Debug.Trace              (trace)

decreaseFreq :: Int -> Map.Map Int Int -> Map.Map Int Int
decreaseFreq n mp
  | curFreq == 1 = Map.delete n mp
  | otherwise = Map.insert n (curFreq - 1) mp
  where
  curFreq =   mp Map.! n

increaseFreq :: Int -> Map.Map Int Int -> Map.Map Int Int
increaseFreq n mp =  Map.insert n (curFreq + 1) mp
  where
  curFreq =  Map.findWithDefault 0 n mp

solve :: Int -> [Int] -> [Int]
solve x queries = fst . Map.findMax . snd <$> drop 1 (scanl op (initPos, initFreq) queries)
  where
  initFreq = Map.singleton x 1
  initPos = Set.fromList [0, x]
  op (pos, freq) query =  (newPos, newFreq)
    where
    left = fromJust $ Set.lookupLT query pos
    right = fromJust $ Set.lookupGT query pos
    newPos = Set.insert query pos
    newFreq = increaseFreq (query - left)
            $ increaseFreq (right - query)
            $ decreaseFreq (right - left) freq

main :: IO ()
main = do
  (x, _) <- getInt2
  queries <- getInts
  mapM_ print $ solve x queries

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

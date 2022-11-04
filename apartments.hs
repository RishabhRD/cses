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
import qualified Data.ByteString.Char8 as C
import           Data.Ix               (Ix)
import           Data.List             (sort, unfoldr)

solve :: (Ord a, Num a, Num t) => a -> [a] -> [a] -> t
solve k as' bs' = solve' (sort as') (sort bs')
  where
  solve' (a : as) (b : bs) = solveWithCompare (compare a b) (a : as) (b : bs)
  solve' _ _               = 0
  solveWithCompare EQ (a : as) (b : bs) = 1 + solve' as bs
  solveWithCompare LT (a : as) (b : bs) = solve' (a : as) bs
  solveWithCompare GT (a : as) (b : bs) = solve' as (b : bs)
  compare a b
    | a - k <= b && b <= a + k = EQ
    | b < a - k = LT
    | otherwise = GT

main :: IO ()
main = do
  [_, _, k] <- getInts
  as <- getInts
  bs <- getInts
  print $ solve k as bs

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

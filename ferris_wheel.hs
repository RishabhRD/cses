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

solve :: Int -> [Int] -> Int
solve k xs = solve' lst rlst
  where
  lst = zip [1..] $ sort xs
  rlst = reverse lst
  solve' ((il, hxl) : xl) ((ir, hxr) : xr)
    | il == ir = 1
    | il > ir = 0
    | hxl + hxr <= k = 1 + solve' xl xr
    | otherwise = 1 + solve' ((il, hxl) : xl) xr

main :: IO ()
main = do
  (_, x) <- getInt2
  xs <- getInts
  print $ solve x xs


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
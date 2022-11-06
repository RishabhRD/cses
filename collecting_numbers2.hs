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

swapVal :: Int -> Int -> IntMap.IntMap a -> IntMap.IntMap a
swapVal x y map = IntMap.insert y xVal $ IntMap.insert x yVal map
  where
  xVal = map IntMap.! x
  yVal = map IntMap.! y

solve :: [Int] -> [(Int, Int)] -> [Int]
solve xs query =  drop 1 $ fst <$> scanl op (initVal, initPos) query
  where
  n = length xs
  initPos = IntMap.fromList $ zip xs [1..] ++ [(0, n + 1)]
  initVal = n - length (filter (`isValid` initPos) xs)
  isValid x pos = pos IntMap.! x > pos IntMap.! (x - 1)
  swapValue True False = -1
  swapValue False True = 1
  swapValue _ _        = 0
  op (curVal, curPos) (x, y) = (curVal + swapValue xBeforeSwap xAfterSwap + swapValue yBeforeSwap yAfterSwap, newPos)
    where
    xBeforeSwap = isValid x curPos
    yBeforeSwap = isValid y curPos
    newPos = swapVal x y curPos
    xAfterSwap = isValid x newPos
    yAfterSwap = isValid y newPos

-- >>> solve [4, 2, 1, 5, 3] [(2, 3), (1, 5), (2, 3)]
-- [3,3,3]

doCase :: IO ()
doCase = do
  return ()

main :: IO ()
main = getInt >>= flip replicateM_ doCase

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

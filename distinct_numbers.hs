module Main where
import           Control.Arrow ((>>>))
import           Data.List     (group, sort)

solve :: [Int] -> Int
solve = length. group . sort

main :: IO ()
main = do
  interact $ input >>> solve >>> show
  return ()

input :: String -> [Int]
input = lines >>> (!!1) >>> words >>> fmap read

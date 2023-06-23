module Main (main) where

import HordeAd
import Prelude

arr :: Array 1
arr = buildSh (C Z) (Arr 0)

testConcatBuild :: IO ()
testConcatBuild = print arr

list :: SizedList 5
list = C (C (C (C (C Z))))

nestedBuildMap :: Array 5
nestedBuildMap = buildSh list (Arr 0)

testNestedBuildMap7 :: IO ()
testNestedBuildMap7 = print (rev nestedBuildMap 0.6)

main :: IO ()
main = sequence_
  [ putStrLn "1"
  , testConcatBuild
  , putStrLn "2"
  , testNestedBuildMap7
  ]

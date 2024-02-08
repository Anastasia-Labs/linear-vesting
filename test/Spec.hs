module Main (main) where

import Test.Tasty (defaultMain)

import LinearVestingSpec qualified

main :: IO ()
main =
  defaultMain LinearVestingSpec.tests

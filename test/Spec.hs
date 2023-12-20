module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import LinearVestingSpec qualified

main :: IO ()
main =
  defaultMain $ testGroup "linear-vesting" [LinearVestingSpec.tests]

{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main (main) where

import LinearVesting
import Utils (writePlutusScript)

import System.Directory (createDirectoryIfMissing, doesDirectoryExist)

main :: IO ()
main = do
  exist <- doesDirectoryExist "compiled"
  createDirectoryIfMissing exist "compiled"
  writePlutusScript "Linear Vesting Validator" "./compiled/linearVesting.json" $ LinearVesting.pvalidateVestingScriptValidator

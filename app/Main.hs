module Main (main) where

import System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleLayer (..), SGR (..), setSGR)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)

import LinearVesting
import Utils (writePlutusScript)

main :: IO ()
main = do
  setSGR [SetColor Foreground Vivid Blue]
  putStrLn "Exporting Plutarch scripts..."
  setSGR [Reset]

  exist <- doesDirectoryExist "compiled"
  createDirectoryIfMissing exist "compiled"
  writePlutusScript "Linear Vesting Validator" "./compiled/linearVesting.json" $ LinearVesting.pvalidateVestingScriptValidator
  putStrLn "Exported linear vesting validator"

  setSGR [SetColor Foreground Vivid Green]
  putStrLn "Done exporting Plutarch scripts, have a great day!"
  setSGR [Reset]

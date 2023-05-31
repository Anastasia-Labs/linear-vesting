module Main (main) where

import Data.Default (
  def,
 )
import LinearVesting qualified
import Ply.Plutarch (
  writeTypedScript,
 )

main :: IO ()
main = do
  writeTypedScript def "test" "./linearVesting.plutus" LinearVesting.pvalidateVestingScriptValidator

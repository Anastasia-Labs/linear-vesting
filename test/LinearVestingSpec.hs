module LinearVestingSpec (tests) where

import Data.Either (fromRight)
import Data.Ratio ((%))

import PlutusLedgerApi.V1 (Address (..))
import PlutusLedgerApi.V1.Value (assetClass, singleton)
import PlutusLedgerApi.V2 (Credential (..), Extended (..), Interval (..), LowerBound (..), POSIXTime (..), PubKeyHash, ScriptContext, UpperBound (..))

import Plutarch (Script, pcon)
import Plutarch.Api.V2 (scriptHash)
import Plutarch.Context (UTXO, address, buildSpending', input, output, script, signedWith, timeRange, withDatum, withRedeemer, withSpendingUTXO, withValue)
import Plutarch.Lift (pconstant)
import Plutarch.Prelude (PUnit (..), (#), (#==))
import Plutarch.Test.QuickCheck (fromPFun)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Gen, Property, chooseInteger, forAll, suchThat, testProperty)

import Compilation
import LinearVesting
import Plutarch.Test.Precompiled (Expectation (Failure, Success), testEvalCase, tryFromPTerm)
import PlutusLedgerApi.V1 qualified as PlutusTx

tests :: TestTree
tests =
  testGroup
    "Linear Vesting Tests"
    [ partialUnlockTests
    , unitTest
    ]

alicePKH :: PubKeyHash
alicePKH = "86ae9eebd8b97944a45201e4aec1330a72291af2d071644bba015959"

alice :: Credential
alice = PubKeyCredential alicePKH

gen_vestingDatum :: Gen VestingDatum
gen_vestingDatum = do
  quantity <- chooseInteger (1, 1_000_000_000_000)
  start <- chooseInteger (timeFrom, timeUntil - 1)
  end <- chooseInteger (start, timeUntil)
  first <- chooseInteger (start, end - 1)
  nr <- chooseInteger (2, 100)
  return $
    VestingDatum
      { beneficiary = Address alice Nothing
      , vestingAsset = assetClass "" ""
      , totalVestingQty = quantity
      , vestingPeriodStart = start
      , vestingPeriodEnd = end
      , firstUnlockPossibleAfter = first
      , totalInstallments = nr
      }
  where
    timeFrom = 1_702_304_208
    timeUntil = 2_702_304_208

data PartialUnlockParameters = PartialUnlockParameters
  { datum :: VestingDatum
  , now :: Integer
  , remaining :: Integer
  }
  deriving stock (Show)

gen_correctPartialUnlockParameters :: Gen PartialUnlockParameters
gen_correctPartialUnlockParameters = do
  vestingDatum <- gen_vestingDatum `suchThat` firstRealUnlockTimeIsBeforeEnd
  let (end, nr, stepLength, realFirstUnlockTime) = calcValues vestingDatum
  let q = totalVestingQty vestingDatum

  now <- chooseInteger (realFirstUnlockTime, end - 1)

  let remainingSteps = ceiling ((end - now) % stepLength)
      remaining = ceiling $ (remainingSteps * q) % nr
  return $
    PartialUnlockParameters
      { datum = vestingDatum
      , now = now
      , remaining = remaining
      }
  where
    calcValues vestingDatum =
      let end = vestingPeriodEnd vestingDatum
          start = vestingPeriodStart vestingDatum
          nr = totalInstallments vestingDatum
          length = end - start
          stepLength = ceiling (length % nr)
          firstUnlockTime = firstUnlockPossibleAfter vestingDatum
          firstUnlockInterval = ceiling ((firstUnlockTime - start) % stepLength)
          realFirstUnlockTime = firstUnlockInterval * stepLength + start
       in (end, nr, stepLength, realFirstUnlockTime)
    firstRealUnlockTimeIsBeforeEnd vestingDatum =
      let (end, _, _, realFirstUnlockTime) = calcValues vestingDatum
       in realFirstUnlockTime < end

vestingScript :: Script
vestingScript = fromRight undefined $ compileTerm $ pvalidateVestingScriptValidator

partialUnlockVestingInputUTxO :: VestingDatum -> UTXO
partialUnlockVestingInputUTxO datum =
  mconcat
    [ script $ scriptHash vestingScript
    , withValue $ singleton "" "" (totalVestingQty datum)
    , withRedeemer PartialUnlock
    , withDatum datum
    ]

partialUnlockVestingOutputUTxO :: PartialUnlockParameters -> UTXO
partialUnlockVestingOutputUTxO parameters =
  mconcat
    [ script $ scriptHash vestingScript
    , withValue $ singleton "" "" (remaining parameters)
    , withDatum $ datum parameters
    ]

partialUnlockScriptContext :: PartialUnlockParameters -> ScriptContext
partialUnlockScriptContext parameters =
  let inputUTxO = partialUnlockVestingInputUTxO $ datum parameters
   in buildSpending' $
        mconcat
          [ timeRange $ Interval (LowerBound (Finite $ POSIXTime $ now parameters) True) (UpperBound PosInf False)
          , input inputUTxO
          , output $ partialUnlockVestingOutputUTxO parameters
          , withSpendingUTXO inputUTxO
          , signedWith alicePKH
          ]

property_partialUnlock_remainingLinearity :: Property
property_partialUnlock_remainingLinearity = forAll gen_correctPartialUnlockParameters $ \parameters ->
  fromPFun $
    pcon PUnit #== pvalidateVestingPartialUnlock # (pconstant $ datum parameters) # (pconstant $ partialUnlockScriptContext parameters)

partialUnlockTests :: TestTree
partialUnlockTests =
  testGroup "Partial Unlock" [testProperty "succeeds with correct parameters" property_partialUnlock_remainingLinearity]

vestingDatum :: VestingDatum
vestingDatum =
  VestingDatum
    { beneficiary = Address alice Nothing
    , vestingAsset = assetClass "" ""
    , totalVestingQty = 10_000_000
    , vestingPeriodStart = 100_000_000
    , vestingPeriodEnd = 150_000_000
    , firstUnlockPossibleAfter = 110_000_000
    , totalInstallments = 10
    }

fullUnlockInputUTxO :: VestingDatum -> UTXO
fullUnlockInputUTxO datum =
  mconcat
    [ script $ scriptHash vestingScript
    , withValue $ singleton "" "" (totalVestingQty datum)
    , withRedeemer FullUnlock
    , withDatum datum
    ]

fullUnlockOutputUTxO :: UTXO
fullUnlockOutputUTxO =
  mconcat
    [ address $ Address alice Nothing
    , withValue $ singleton "" "" (totalVestingQty vestingDatum)
    ]

fullUnlockScriptContext :: ScriptContext
fullUnlockScriptContext =
  let inputUTxO = fullUnlockInputUTxO vestingDatum
   in buildSpending' $
        mconcat
          [ timeRange $ Interval (LowerBound (Finite $ POSIXTime $ 160_000_000) True) (UpperBound PosInf False)
          , input inputUTxO
          , output fullUnlockOutputUTxO
          , withSpendingUTXO inputUTxO
          , signedWith alicePKH
          ]

invalidFullUnlockScriptContext :: ScriptContext
invalidFullUnlockScriptContext =
  let inputUTxO = fullUnlockInputUTxO vestingDatum
   in buildSpending' $
        mconcat
          [ timeRange $ Interval (LowerBound (Finite $ POSIXTime $ 140_000_000) True) (UpperBound PosInf False)
          , input inputUTxO
          , output fullUnlockOutputUTxO
          , withSpendingUTXO inputUTxO
          , signedWith alicePKH
          ]

unitTest :: TestTree
unitTest = tryFromPTerm "Unit Tests" pvalidateVestingScriptValidator $ do
  testEvalCase
    "Pass - Full Unlock"
    Success
    [ PlutusTx.toData vestingDatum
    , PlutusTx.toData FullUnlock
    , PlutusTx.toData fullUnlockScriptContext
    ]
  testEvalCase
    "Fail - Full Unlock - Before End"
    Failure
    [ PlutusTx.toData vestingDatum
    , PlutusTx.toData FullUnlock
    , PlutusTx.toData invalidFullUnlockScriptContext
    ]

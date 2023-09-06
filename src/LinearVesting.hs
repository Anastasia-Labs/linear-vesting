{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module LinearVesting (pvalidateVestingScriptValidator) where

import Plutarch.Api.V1 (PAddress, PCredential (PPubKeyCredential))
import Plutarch.Api.V2 (PPOSIXTime, PScriptContext, PScriptPurpose (PSpending), PValidator)
import Plutarch.DataRepr (PDataFields)
import Plutarch.Extra.AssetClass (PAssetClassData, ptoScottEncoding)
import Plutarch.Extra.ScriptContext (pfindOutputsToAddress, pfindOwnInput, ptxSignedBy)
import "plutarch-extra" Plutarch.Extra.TermCont (pguardC, pletC, pletFieldsC, pmatchC, ptraceC, ptryFromC)
import Plutarch.Extra.Time (PFullyBoundedTimeRange (PFullyBoundedTimeRange), passertFullyBoundedTimeRange)
import Plutarch.Extra.Value (passetClassValueOf)
import Plutarch.Positive (ptryPositive)
import Plutarch.Prelude
import Utils (pdivCeil, pgetLowerInclusiveTimeRange, pheadSingleton)

data PVestingDatum (s :: S)
  = PVestingDatum
      ( Term
          s
          ( PDataRecord
              '[ "beneficiary" ':= PAddress
               , "vestingAsset" ':= PAssetClassData
               , "totalVestingQty" ':= PInteger
               , "vestingPeriodStart" ':= PInteger
               , "vestingPeriodEnd" ':= PInteger
               , "firstUnlockPossibleAfter" ':= PInteger
               , "totalInstallments" ':= PInteger
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PVestingDatum where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData (PAsData PAssetClassData)

instance PTryFrom PData PVestingDatum

instance PTryFrom PData (PAsData PVestingDatum)

data PVestingRedeemer (s :: S)
  = PPartialUnlock (Term s (PDataRecord '[]))
  | PFullUnlock (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType)

instance DerivePlutusType PVestingRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PVestingRedeemer

pvalidateVestingPartialUnlock ::
  Term
    s
    ( PVestingDatum
        :--> PScriptContext
        :--> PUnit
    )
pvalidateVestingPartialUnlock = phoistAcyclic $ plam $ \datum ctx -> unTermCont $ do
  ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx
  txInfoF <- pletFieldsC @'["outputs", "inputs", "signatories", "validRange"] ctxF.txInfo
  PSpending ((pfield @"_0" #) -> ownRef) <- pmatchC ctxF.purpose

  PJust ownVestingInput <- pmatchC $ pfindOwnInput # txInfoF.inputs # ownRef
  ownVestingInputF <- pletFieldsC @'["address", "value", "datum"] (pfield @"resolved" # ownVestingInput)

  ownVestingOutput <- pletC $ pheadSingleton #$ pfindOutputsToAddress # txInfoF.outputs # ownVestingInputF.address
  ownVestingOutputF <- pletFieldsC @'["address", "value", "datum"] ownVestingOutput

  datumF <-
    pletFieldsC
      @'[ "beneficiary"
        , "vestingAsset"
        , "totalVestingQty"
        , "vestingPeriodStart"
        , "vestingPeriodEnd"
        , "firstUnlockPossibleAfter"
        , "totalInstallments"
        ]
      datum

  vestingAsset <- pletC $ ptoScottEncoding # datumF.vestingAsset
  currentTimeApproximation <- pletC $ pfromData $ pto $ pgetLowerInclusiveTimeRange # txInfoF.validRange

  oldRemainingQty <- pletC $ passetClassValueOf # vestingAsset # ownVestingInputF.value
  newRemainingQty <- pletC $ passetClassValueOf # vestingAsset # ownVestingOutputF.value
  vestingPeriodLength <- pletC $ (pfromData datumF.vestingPeriodEnd) - (pfromData datumF.vestingPeriodStart)
  vestingTimeRemaining <- pletC $ (pfromData datumF.vestingPeriodEnd) - (currentTimeApproximation)
  timeBetweenTwoInstallments <- pletC $ pdivCeil # vestingPeriodLength # datumF.totalInstallments
  futureInstallments <- pletC $ pdivCeil # (vestingTimeRemaining) # (timeBetweenTwoInstallments)

  let expectedRemainingQty = pdivCeil # (futureInstallments * datumF.totalVestingQty) #$ datumF.totalInstallments

  PPubKeyCredential ((pfield @"_0" #) -> beneficiaryHash) <- pmatchC (pfield @"credential" # datumF.beneficiary)

  pguardC "Missing beneficiary signature" (ptxSignedBy # txInfoF.signatories # beneficiaryHash)
  pguardC "Unlock not permitted until firstUnlockPossibleAfter time" (datumF.firstUnlockPossibleAfter #< currentTimeApproximation)
  pguardC "Zero remaining assets not allowed" (0 #< newRemainingQty)
  pguardC "Remaining asset exceed old asset" (newRemainingQty #< oldRemainingQty)
  pguardC "Mismatched remaining asset" (expectedRemainingQty #== newRemainingQty)
  pguardC "Datum Modification Prohibited" (ownVestingInputF.datum #== ownVestingOutputF.datum)
  pure $ pconstant ()

pvalidateVestingFullUnlock :: Term s (PVestingDatum :--> PScriptContext :--> PUnit)
pvalidateVestingFullUnlock = phoistAcyclic $ plam $ \datum context -> unTermCont $ do
  datumF <- tcont $ pletFields @'["beneficiary", "vestingPeriodEnd"] datum
  txInfoF <- tcont $ pletFields @'["signatories", "validRange"] $ pfield @"txInfo" # context
  currentTimeApproximation <- pletC $ pfromData $ pto $ pgetLowerInclusiveTimeRange # txInfoF.validRange
  PPubKeyCredential ((pfield @"_0" #) -> beneficiaryHash) <- pmatchC (pfield @"credential" # datumF.beneficiary)

  pguardC "Missing beneficiary signature" (ptxSignedBy # txInfoF.signatories # beneficiaryHash)
  pguardC "Unlock not permitted until vestingPeriodEnd time" (datumF.vestingPeriodEnd #< currentTimeApproximation)
  pure $ pconstant ()

pvalidateVestingScript ::
  Term
    s
    ( PVestingDatum
        :--> PVestingRedeemer
        :--> PScriptContext
        :--> PUnit
    )
pvalidateVestingScript =
  phoistAcyclic $
    plam $
      \datum redeemer context -> unTermCont $ do
        pure $
          pmatch redeemer $ \case
            PPartialUnlock _ -> pvalidateVestingPartialUnlock # datum # context
            PFullUnlock _ -> pvalidateVestingFullUnlock # datum # context

pvalidateVestingScriptValidator :: Term s PValidator
pvalidateVestingScriptValidator = phoistAcyclic $
  plam $ \dat red ctx -> unTermCont $ do
    (datum, _) <- ptryFromC @PVestingDatum dat
    (redeemer, _) <- ptryFromC @PVestingRedeemer red
    pure $
      popaque $
        pvalidateVestingScript # datum # redeemer # ctx

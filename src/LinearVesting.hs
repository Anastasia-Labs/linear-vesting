module LinearVesting (
  PVestingDatum (..),
  PVestingRedeemer (..),
  VestingDatum (..),
  VestingRedeemer (..),
  pvalidateVestingPartialUnlock,
  pvalidateVestingScriptValidator,
) where

import PlutusLedgerApi.V1.Value (AssetClass)
import PlutusLedgerApi.V2 (Address)
import PlutusTx qualified

import Plutarch.Api.V1 (PAddress, PCredential (..))
import Plutarch.Api.V2 (PCurrencySymbol, PScriptContext, PScriptHash, PScriptPurpose (PSpending), PTokenName, PTxInInfo, PValidator)
import Plutarch.DataRepr (DerivePConstantViaData (..), PDataFields)
import Plutarch.Extra.AssetClass (PAssetClass (..))
import Plutarch.Extra.ScriptContext (pfindOutputsToAddress, pfindOwnInput, ptxSignedBy)
import Plutarch.Extra.Value (passetClassValueOf)
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (pguardC, pletC, pletFieldsC, pmatchC)

import Conversions
import Utils

data VestingDatum = VestingDatum
  { beneficiary :: Address
  , vestingAsset :: AssetClass
  , totalVestingQty :: Integer
  , vestingPeriodStart :: Integer
  , vestingPeriodEnd :: Integer
  , firstUnlockPossibleAfter :: Integer
  , totalInstallments :: Integer
  }
  deriving stock (Show)

PlutusTx.makeLift ''VestingDatum
PlutusTx.makeIsDataIndexed ''VestingDatum [('VestingDatum, 0)]

data PAssetClassData (s :: S) = PAssetClassData (Term s (PDataRecord '["symbol" ':= PCurrencySymbol, "tokenName" ':= PTokenName]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PAssetClassData where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PAssetClassData

ptoScottEncoding :: Term s (PAssetClassData :--> PAssetClass)
ptoScottEncoding = phoistAcyclic $ plam $ \x' -> P.do
  x <- pletFields @["symbol", "tokenName"] x'
  pcon $ PAssetClass x.symbol x.tokenName

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

instance PTryFrom PData PVestingDatum

instance PUnsafeLiftDecl PVestingDatum where type PLifted PVestingDatum = VestingDatum
deriving via (DerivePConstantViaData VestingDatum PVestingDatum) instance PConstantDecl VestingDatum

data VestingRedeemer
  = PartialUnlock
  | FullUnlock

PlutusTx.makeLift ''VestingRedeemer
PlutusTx.makeIsDataIndexed ''VestingRedeemer [('PartialUnlock, 0), ('FullUnlock, 1)]

data PVestingRedeemer (s :: S)
  = PPartialUnlock (Term s (PDataRecord '[]))
  | PFullUnlock (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PVestingRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PVestingRedeemer

instance PUnsafeLiftDecl PVestingRedeemer where type PLifted PVestingRedeemer = VestingRedeemer
deriving via (DerivePConstantViaData VestingRedeemer PVestingRedeemer) instance PConstantDecl VestingRedeemer

pcountInputsAtScript :: Term s (PScriptHash :--> PBuiltinList PTxInInfo :--> PInteger)
pcountInputsAtScript =
  phoistAcyclic $ plam $ \sHash ->
    let go :: Term _ (PInteger :--> PBuiltinList PTxInInfo :--> PInteger)
        go = pfix #$ plam $ \self n ->
          pelimList
            ( \x xs ->
                let cred = pfield @"credential" # (pfield @"address" # (pfield @"resolved" # x))
                 in pmatch cred $ \case
                      PScriptCredential ((pfield @"_0" #) -> vh) -> pif (sHash #== vh) (self # (n + 1) # xs) (self # n # xs)
                      _ -> self # n # xs
            )
            n
     in go # 0

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
  PScriptCredential ((pfield @"_0" #) -> ownValHash) <- pmatchC (pfield @"credential" # ownVestingInputF.address)

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
  pguardC "Remaining asset exceed old asset" $ ptrace (pshow datumF.totalInstallments) $ (newRemainingQty #< oldRemainingQty)
  pguardC "Mismatched remaining asset" (expectedRemainingQty #== newRemainingQty)
  pguardC "Datum Modification Prohibited" (ownVestingInputF.datum #== ownVestingOutputF.datum)
  pguardC "Double satisfaction" (pcountInputsAtScript # ownValHash # txInfoF.inputs #== 1)
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
    let datum = pconvert dat
    let redeemer = pconvert red
    return $
      popaque $
        pvalidateVestingScript # datum # redeemer # ctx

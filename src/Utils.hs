module Utils (
  pheadSingleton,
  pdivCeil,
  pgetInclusiveTimeRange,
  pgetLowerInclusiveTimeRange,
  (#>=),
  (#/=),
  (#>),
  writePlutusScript,
  writePlutusScriptWithTracing,
  evalT,
)
where

import Cardano.Binary qualified as CBOR
import Data.Aeson (KeyValue ((.=)), object)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Bifunctor (
  first,
 )
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LBS
import Data.Text (
  Text,
  pack,
 )
import Data.Text.Encoding qualified as Text
import Plutarch (
  Config (Config),
  TracingMode (..),
  compile,
 )
import Plutarch.Api.V1 (PExtended (PFinite), PInterval (PInterval), PLowerBound (PLowerBound), PPOSIXTimeRange, PUpperBound (PUpperBound))
import Plutarch.Api.V1.Time (PPOSIXTime)
import Plutarch.Builtin (ppairDataBuiltin)
import Plutarch.Evaluate (
  evalScript,
 )
import Plutarch.Num (PNum)
import Plutarch.Prelude
import Plutarch.Script (Script, serialiseScript)
import PlutusLedgerApi.V2 (
  Data,
  ExBudget,
 )
import "liqwid-plutarch-extra" Plutarch.Extra.Script (
  applyArguments,
 )
import "plutarch-extra" Plutarch.Extra.TermCont (pletC, pletFieldsC, pmatchC)

(#>) :: (POrd t) => Term s t -> Term s t -> Term s PBool
a #> b = b #< a

infix 4 #>

(#>=) :: (POrd t) => Term s t -> Term s t -> Term s PBool
a #>= b = b #<= a

infix 4 #>=

(#/=) :: (PEq t) => Term s t -> Term s t -> Term s PBool
a #/= b = pnot # (a #== b)

infix 4 #/=

pheadSingleton :: (PListLike list, PElemConstraint list a) => Term s (list a :--> a)
pheadSingleton = phoistAcyclic $
  plam $ \xs ->
    pelimList (\x xs -> pif (pnull # xs) x (ptraceError "List contains more than one element.")) perror xs

pdivCeil :: (PIntegral a, PNum a) => Term s (a :--> a :--> a)
pdivCeil = phoistAcyclic $
  plam $
    \x y -> 1 + pdiv # (x - 1) # y

pgetInclusiveTimeRange ::
  forall (s :: S).
  Term
    s
    ( PPOSIXTimeRange
        :--> PBuiltinPair (PAsData PPOSIXTime) (PAsData PPOSIXTime)
    )
pgetInclusiveTimeRange = phoistAcyclic $
  plam $ \timeRange' -> unTermCont $ do
    PInterval timeRange <- pmatchC timeRange'
    intervalF <- pletFieldsC @["from", "to"] timeRange
    PLowerBound lb <- pmatchC intervalF.from
    PUpperBound ub <- pmatchC intervalF.to

    let tryBound = phoistAcyclic $
          plam $ \endpoint a ->
            pmatch (pfield @"_0" # a) $ \case
              PFinite ((pfield @"_0" #) -> posixTime) ->
                pif
                  (pfield @"_1" # a)
                  (posixTime)
                  (pdata $ (pfromData posixTime) + endpoint)
              _ -> ptraceError "Time range not Finite"

    inclusiveLb <- pletC $ tryBound # (1) # lb
    inclusiveUp <- pletC $ tryBound # (-1) # ub

    -- if both intervals are the same and upper bound is not inclusive (closure false) then it becomes -> upper bound < lower bound
    pure $
      pif
        (pfromData inclusiveLb #< pfromData inclusiveUp)
        (ppairDataBuiltin # inclusiveLb # inclusiveUp)
        (ptraceError "Not inclusive Bound")

pgetLowerInclusiveTimeRange ::
  forall (s :: S).
  Term
    s
    ( PPOSIXTimeRange
        :--> (PAsData PPOSIXTime)
    )
pgetLowerInclusiveTimeRange = phoistAcyclic $
  plam $ \timeRange -> unTermCont $ do
    PInterval ((pfield @"from" #) -> from) <- pmatchC timeRange
    PLowerBound lb <- pmatchC from

    let tryBound = phoistAcyclic $
          plam $ \endpoint a ->
            pmatch (pfield @"_0" # a) $ \case
              PFinite ((pfield @"_0" #) -> posixTime) ->
                pif
                  (pfield @"_1" # a)
                  (posixTime)
                  (pdata $ (pfromData posixTime) + endpoint)
              _ -> ptraceError "Time range not Finite"
    inclusiveLb <- pletC $ tryBound # (1) # lb
    pure $ inclusiveLb

encodeSerialiseCBOR :: Script -> Text
encodeSerialiseCBOR = Text.decodeUtf8 . Base16.encode . CBOR.serialize' . serialiseScript

evalT :: Config -> ClosedTerm a -> Either Text (Script, ExBudget, [Text])
evalT cfg x = evalWithArgsT cfg x []

evalWithArgsT :: Config -> ClosedTerm a -> [Data] -> Either Text (Script, ExBudget, [Text])
evalWithArgsT cfg x args = do
  cmp <- compile cfg x
  let (escr, budg, trc) = evalScript $ applyArguments cmp args
  scr <- first (pack . show) escr
  pure (scr, budg, trc)

writePlutusScript :: String -> FilePath -> ClosedTerm a -> IO ()
writePlutusScript title filepath term = do
  case evalT (Config NoTracing) term of
    Left e -> putStrLn (show e)
    Right (script, _, _) -> do
      let
        scriptType = "PlutusScriptV2" :: String
        plutusJson = object ["type" .= scriptType, "description" .= title, "cborHex" .= encodeSerialiseCBOR script]
        content = encodePretty plutusJson
      LBS.writeFile filepath content

writePlutusScriptWithTracing :: String -> FilePath -> ClosedTerm a -> IO ()
writePlutusScriptWithTracing title filepath term = do
  case evalT (Config DoTracing) term of
    Left e -> putStrLn (show e)
    Right (script, _, _) -> do
      let
        scriptType = "PlutusScriptV2" :: String
        plutusJson = object ["type" .= scriptType, "description" .= title, "cborHex" .= encodeSerialiseCBOR script]
        content = encodePretty plutusJson
      LBS.writeFile filepath content

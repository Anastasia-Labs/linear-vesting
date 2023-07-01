module Utils (
  pheadSingleton,
  pdivCeil,
  pgetInclusiveTimeRange,
  pgetLowerInclusiveTimeRange,
  (#>=),
  (#/=),
  (#>),
)
where

import Plutarch.Api.V1 (PExtended (PFinite), PInterval (PInterval), PLowerBound (PLowerBound), PPOSIXTimeRange, PUpperBound (PUpperBound))
import Plutarch.Api.V1.Time (PPOSIXTime)
import Plutarch.Builtin (ppairDataBuiltin)
import "plutarch-extra" Plutarch.Extra.TermCont (pletC, pletFieldsC, pmatchC)
import Plutarch.Num (PNum)
import Plutarch.Prelude

(#>) :: POrd t => Term s t -> Term s t -> Term s PBool
a #> b = b #< a

infix 4 #>

(#>=) :: (POrd t) => Term s t -> Term s t -> Term s PBool
a #>= b = b #<= a

infix 4 #>=

(#/=) :: PEq t => Term s t -> Term s t -> Term s PBool
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

module Utils (
  pheadSingleton,
  pdivCeil,
)
where

import Plutarch.Num (PNum)
import Plutarch.Prelude

pheadSingleton :: (PListLike list, PElemConstraint list a) => Term s (list a :--> a)
pheadSingleton = phoistAcyclic $
  plam $ \xs ->
    pelimList (\x xs -> pif (pnull # xs) x (ptraceError "List contains more than one element.")) perror xs

pdivCeil :: (PIntegral a, PNum a) => Term s (a :--> a :--> a)
pdivCeil = phoistAcyclic $
  plam $
    \x y -> 1 + pdiv # (x - 1) # y

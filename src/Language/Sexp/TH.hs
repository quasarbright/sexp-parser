module Language.Sexp.TH where

import Data.Generics
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Language.Sexp.AParser
import Data.Sexp

sexp :: QuasiQuoter
sexp =
  QuasiQuoter
    { quoteExp = quoteSexpExp,
      quotePat = quoteSexpPat,
      quoteType = undefined,
      quoteDec = undefined
    }

qSexp :: String -> Q ASexp
qSexp s = do
  loc <- location
  let pos =
        ( loc_filename loc,
          fst (loc_start loc),
          snd (loc_start loc)
        )
  parseASexpAt pos s

quoteSexpExp :: String -> ExpQ
quoteSexpExp s = do
  expr <- qSexp s
  dataToExpQ (const Nothing `extQ` antiSexpExp) expr

antiSexpExp :: ASexp -> Maybe ExpQ
antiSexpExp (AntiSexp x) = Just $ varE (mkName x)
antiSexpExp _ = Nothing

quoteSexpPat :: String -> PatQ
quoteSexpPat s = do
  expr <- qSexp s
  dataToPatQ (const Nothing `extQ` antiSexpPat) expr

antiSexpPat :: ASexp -> Maybe PatQ
antiSexpPat (AntiSexp x) = Just $ varP (mkName x)
antiSexpPat _ = Nothing

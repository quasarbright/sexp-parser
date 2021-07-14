{-# LANGUAGE QuasiQuotes #-}
module Lib where

import Language.Sexp.TH
import Data.Sexp

is1 :: ASexp -> Bool
is1 [sexp|1|] = True
is1 _ = False

getArgnames :: ASexp -> Maybe [ASexp]
getArgnames [sexp|(lambda {argnamesExp} {body})|]
    | AList argnames <- argnamesExp = Just argnames
    | otherwise = Nothing
getArgnames _ = Nothing

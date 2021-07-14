{-# LANGUAGE QuasiQuotes #-}
module Main where

import Lib
import Language.Sexp.TH
import Data.Sexp

main :: IO ()
main = print (is1 [sexp|1|]) >> print (is1 [sexp|(1)|]) >> print (getArgnames [sexp| (lambda (x y z) x)|])

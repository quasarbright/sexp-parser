{-# LANGUAGE DeriveDataTypeable #-}
module Data.Sexp where

import Data.Generics

data ASexp
    = AList [ASexp]
    | AAtom String
    -- special forms
    | AInteger Integer
    | AString String
    -- anti forms
    | AntiSexp String
    | AntiList String
    | AntiAtom String
    | AntiInteger String
    | AntiString String
    deriving(Eq, Ord, Data, Typeable, Show)

data Sexp
    = SList [Sexp]
    | SAtom String
    -- special forms
    | SInteger Integer
    | SString String
    deriving(Eq, Ord, Data, Typeable)

instance Show Sexp where
    show e_ = case e_ of
        SList es -> "("++unwords (fmap show es)++")"
        SAtom s -> s
        SInteger n -> show n
        SString s -> show s

toSexp :: ASexp -> Maybe Sexp
toSexp ae = case ae of
    AList aes -> SList <$> traverse toSexp aes
    AAtom a -> Just $ SAtom a
    AInteger n -> Just $ SInteger n
    AString s -> Just $ SString s
    AntiList{} -> Nothing
    AntiAtom{} -> Nothing
    AntiInteger{} -> Nothing
    AntiSexp{} -> Nothing
    AntiString{} -> Nothing
    
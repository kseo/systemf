module Language.LambdaCalculus.AST.Term
  ( Info(..)
  , Term(..)
  ) where

import Language.LambdaCalculus.Types

data Term =
    TmVar Info Int Int
  | TmAbs Info String Ty Term
  | TmApp Info Term Term
  | TmIf Info Term Term Term
  | TmTrue Info
  | TmFalse Info
  | TmTAbs Info String Term
  | TmTApp Info Term Ty
  | TmPack Info Ty Term Ty
  | TmUnpack Info String String Term Term
  deriving (Show)

data Info = Info { row :: Int, col :: Int }

instance Show Info where
  show (Info r c) = "(" ++ show r ++ "," ++ show c ++ ")"

module Language.LambdaCalculus.Types
  ( Ty(..)
  ) where

data Ty =
    TyBool
  | TyVar Int Int
  | TyArr Ty Ty
  | TyAll String Ty
  | TySome String Ty
  deriving (Eq, Show)

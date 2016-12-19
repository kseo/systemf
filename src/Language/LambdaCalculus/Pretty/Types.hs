module Language.LambdaCalculus.Pretty.Types
  ( printType
  ) where

import Language.LambdaCalculus.Context
import Language.LambdaCalculus.Types

printType :: Context -> Ty -> String
printType _ TyBool = "Bool"
printType ctx (TyArr t1 t2) = "(" ++ printType ctx t1 ++ "->" ++ printType ctx t2 ++ ")"
printType ctx (TyVar x n) =
    if ctxLength ctx == n then
      indexToName ctx x
    else
      "[bad index]"
printType ctx (TyAll x ty) = let
  (ctx', x') = pickFreshName ctx x
  in "(All " ++ x' ++ "." ++ printType ctx' ty ++ ")"
printType ctx (TySome x ty) = let
  (ctx', x') = pickFreshName ctx x
  in "{Some " ++ x' ++ ", " ++ printType ctx' ty ++ "}"

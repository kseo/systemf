module Language.LambdaCalculus.Pretty.Term
  ( printTm
  ) where

import Language.LambdaCalculus.AST
import Language.LambdaCalculus.Context
import Language.LambdaCalculus.Pretty.Types

printTm :: Context -> Term -> String
printTm ctx t = case t of
  TmAbs _ x ty t1 -> let
      (ctx', x') = pickFreshName ctx x
    in "(\\" ++ x' ++ ":" ++ printType ctx ty ++ "." ++ printTm ctx' t1 ++ ")"
  TmApp _ t1 t2 ->
    "(" ++ printTm ctx t1 ++ " " ++ printTm ctx t2 ++ ")"
  TmVar _ x n ->
    if ctxLength ctx == n then
      indexToName ctx x
    else
      "[bad index]"
  TmTAbs _ tyX t2 -> let
      (ctx', tyX') = pickFreshName ctx tyX
    in "(\\" ++ tyX' ++ "." ++ printTm ctx' t2 ++ ")"
  TmTApp _ t1 tyT2 ->
    "(" ++ printTm ctx t1 ++ ")" ++ " (" ++ printType ctx tyT2 ++ ")"
  TmPack _ tyT1 t2 tyT ->
    "{*" ++ printType ctx tyT1 ++ ", " ++ printTm ctx t2 ++ "} as {" ++ printType ctx tyT ++ "}"
  TmUnpack _ tyX x (TmPack _ tyT1 t1 tyT) t2 ->
    "let {" ++ tyX ++ ", " ++ x ++ "} = " ++ "{*" ++ printType ctx tyT1 ++ ", " ++ printTm ctx t1 ++ "} as {" ++ printType ctx tyT ++ "} in (" ++ printTm ctx t2 ++ ")"
  TmUnpack{} -> error "invalid term"
  TmIf _ t1 t2 t3 ->
    "if" ++ " " ++ printTm ctx t1 ++ " then " ++ printTm ctx t2 ++ " else " ++ printTm ctx t3
  TmTrue _ -> "true"
  TmFalse _ -> "false"

module Language.LambdaCalculus.Context
  ( Binding(..)
  , Context
  , addBinding
  , ctxLength
  , getBinding
  , getTypeFromContext
  , indexToName
  , pickFreshName
  ) where

import Language.LambdaCalculus.Types

data Binding =
    NameBind
  | VarBind Ty
  | TyVarBind
  deriving (Show)

type Context = [(String, Binding)]

ctxLength :: Context -> Int
ctxLength = length

addBinding :: Context -> (String, Binding) -> Context
addBinding = flip (:)

getTypeFromContext :: Context -> Int -> Ty
getTypeFromContext ctx n =
  case getBinding ctx n of
    (VarBind tyT) -> tyT
    _ -> error $ "getTypeFromContext: Wrong kind of binding for variable " ++ indexToName ctx n

indexToName :: Context -> Int -> String
indexToName ctx n = fst $ ctx !! n

getBinding :: Context -> Int -> Binding
getBinding ctx n = snd $ ctx !! n

pickFreshName :: Context -> String -> (Context, String)
pickFreshName ctx x
  | x `elem` map fst ctx = pickFreshName ctx $ x ++ "'"
  | otherwise = ((x, NameBind) : ctx , x)

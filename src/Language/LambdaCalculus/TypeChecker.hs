module Language.LambdaCalculus.TypeChecker
  ( TypeError(..)
  , typeOf
  ) where

import Control.Monad.Except

import Language.LambdaCalculus.AST
import Language.LambdaCalculus.Context
import Language.LambdaCalculus.Evaluator
import Language.LambdaCalculus.Types

data TypeError = TypeError Info String

instance Show TypeError where
  show (TypeError fi msg) = msg ++ " at " ++ show fi

typeOf :: (MonadError TypeError m) => Context -> Term -> m Ty
typeOf ctx t = case t of
  TmVar _ i _ -> return (getTypeFromContext ctx i)
  TmAbs _ x tyT1 t2 -> do
    let ctx' = addBinding ctx (x, VarBind tyT1)
    tyT2 <- typeOf ctx' t2
    return (TyArr tyT1 tyT2)
  TmApp fi t1 t2 -> do
    tyT1 <- typeOf ctx t1
    tyT2 <- typeOf ctx t2
    case tyT1 of
      TyArr tyT11 tyT12 ->
        if tyT2 == tyT11
          then return tyT12
          else throwError (TypeError fi "parameter type mismatch")
      _ -> throwError (TypeError fi "arrow type expected")
  TmTAbs _ tyX t2 -> do
    let ctx' = addBinding ctx (tyX, TyVarBind)
    tyT2 <- typeOf ctx' t2
    return $ TyAll tyX tyT2
  TmTApp fi t1 tyT2 -> do
    tyT1 <- typeOf ctx t1
    case tyT1 of
      TyAll _ tyT12 -> return $ typeSubstTop tyT2 tyT12
      _ -> throwError (TypeError fi "universal type expected")
  TmPack fi tyT1 t2 tyT ->
    case tyT of
      TySome _ tyT2 -> do
        tyU <- typeOf ctx t2
        let tyU' = typeSubstTop tyT1 tyT2
        if tyU == tyU' then return tyT
                       else throwError (TypeError fi "doesn't match declared type")
      _ -> throwError (TypeError fi "existential type expected")
  TmUnpack fi tyX x t1 t2 -> do
    tyT1 <- typeOf ctx t1
    case tyT1 of
      TySome _ tyT11 -> do
        let ctx' = addBinding ctx (tyX, TyVarBind)
            ctx'' = addBinding ctx' (x, VarBind tyT11)
        tyT2 <- typeOf ctx'' t2
        return $ typeShift (-2) tyT2
      _ -> throwError (TypeError fi "existential type expected")
  TmTrue _          -> return TyBool
  TmFalse _         -> return TyBool
  TmIf fi t1 t2 t3  -> do
    tyT1 <- typeOf ctx t1
    if tyT1 == TyBool
      then do
        tyT2 <- typeOf ctx t2
        tyT3 <- typeOf ctx t3
        if tyT2 == tyT3
          then return tyT2
          else throwError (TypeError fi "arms of conditional have different types")
      else throwError (TypeError fi "guard of conditional not a boolean")

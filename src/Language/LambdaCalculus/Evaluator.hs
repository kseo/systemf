module Language.LambdaCalculus.Evaluator
  ( eval
  , typeShift
  , typeSubstTop
  ) where

import Language.LambdaCalculus.AST
import Language.LambdaCalculus.Context
import Language.LambdaCalculus.Types

typeMap :: (Int -> Int -> Int -> Ty) -> Int -> Ty -> Ty
typeMap onVar = walk
  where
  walk c tyT = case tyT of
              TyBool -> TyBool
              TyArr tyT1 tyT2 -> TyArr (walk c tyT1) (walk c tyT2)
              TyVar x n -> onVar c x n
              TyAll tyX tyT2 -> TyAll tyX (walk (c + 1) tyT2)
              TySome tyX tyT2 -> TySome tyX (walk (c + 1) tyT2)

typeShiftAbove :: Int -> Int -> Ty -> Ty
typeShiftAbove d = typeMap (\c x n -> if x >= c then
                                                if x + d < 0 then error "Scoping error!"
                                                             else TyVar (x + d) (n + d)
                                                else TyVar x (n + d))

typeShift :: Int -> Ty -> Ty
typeShift d = typeShiftAbove d 0

typeSubst :: Ty -> Int -> Ty -> Ty
typeSubst tyS = typeMap (\j x n -> if x == j then typeShift j tyS else TyVar x n)

typeSubstTop :: Ty -> Ty -> Ty
typeSubstTop tyS tyT = typeShift (-1) (typeSubst (typeShift 1 tyS) 0 tyT)

termMap :: (Info -> Int -> Int -> Int -> Term) -> (Int -> Ty -> Ty) -> Int -> Term -> Term
termMap onVar onType = walk
  where
  walk c t = case t of
    TmVar fi x n        -> onVar fi c x n
    TmAbs fi x tyT1 t2  -> TmAbs fi x (onType c tyT1) (walk (c + 1) t2)
    TmApp fi t1 t2      -> TmApp fi (walk c t1) (walk c t2)
    TmTAbs fi tyX t2    -> TmTAbs fi tyX (walk (c + 1) t2)
    TmTApp fi t1 tyT2   -> TmTApp fi (walk c t1) (onType c tyT2)
    TmPack fi tyT1 t2 tyT3 ->
      TmPack fi (onType c tyT1) (walk c t2) (onType c tyT3)
    TmUnpack fi tyX x t1 t2 ->
      TmUnpack fi tyX x (walk c t1) (walk (c + 2) t2)
    TmIf fi t1 t2 t3    -> TmIf fi (walk c t1) (walk c t2) (walk c t3)
    TmTrue _            -> t
    TmFalse _           -> t

termShiftAbove :: Int -> Int -> Term -> Term
termShiftAbove d =
  termMap (\fi c x n -> if x >= c then TmVar fi (x + d) (n + d)
                                  else TmVar fi x (n + d))
          (typeShiftAbove d)

termShift :: Int -> Term -> Term
termShift d = termShiftAbove d 0

termSubst :: Int -> Term -> Term -> Term
termSubst j s =
  termMap (\fi j' x n -> if x == j' then termShift j' s else TmVar fi x n)
          (\_ tyT -> tyT)
          j

termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

typeTermSubst :: Ty -> Int -> Term -> Term
typeTermSubst tyS =
  termMap (\fi _ x n -> TmVar fi x n)
          (typeSubst tyS)

typeTermSubstTop :: Ty -> Term -> Term
typeTermSubstTop tyS t = termShift (-1) (typeTermSubst (typeShift 1 tyS) 0 t)

isVal :: Context -> Term -> Bool
isVal _ TmTrue{}  = True
isVal _ TmFalse{} = True
isVal _ TmAbs{}   = True
isVal _ TmTAbs{}  = True
isVal _ TmPack{}  = True
isVal _ _ = False

eval1 :: Context -> Term -> Maybe Term
eval1 ctx t = case t of
                TmApp _ (TmAbs _ _ _ t12) v2 | isVal ctx v2 ->
                  return $ termSubstTop v2 t12
                TmApp fi v1 t2 | isVal ctx v1 -> do
                  t2' <- eval1 ctx t2
                  return $ TmApp fi v1 t2'
                TmApp fi t1 t2 -> do
                  t1' <- eval1 ctx t1
                  return $ TmApp fi t1' t2
                TmTApp _ (TmTAbs _ _ t11) tyT2 ->
                  return $ typeTermSubstTop tyT2 t11
                TmTApp fi t1 tyT2 -> do
                  t1' <- eval1 ctx t1
                  return $ TmTApp fi t1' tyT2
                TmUnpack _ _ _ (TmPack _ tyT11 v12 _) t2 | isVal ctx v12 ->
                  return $ typeTermSubstTop tyT11 (termSubstTop (termShift 1 v12) t2)
                TmUnpack fi tyX x t1 t2 -> do
                  t1' <- eval1 ctx t1
                  return $ TmUnpack fi tyX x t1' t2
                TmPack fi tyT1 t2 tyT3 -> do
                  t2' <- eval1 ctx t2
                  return $ TmPack fi tyT1 t2' tyT3
                TmIf _ TmTrue{} t2 _ -> return t2
                TmIf _ TmFalse{} _ t3 -> return t3
                TmIf fi t1 t2 t3 -> do
                  t1' <- eval1 ctx t1
                  return $ TmIf fi t1' t2 t3
                _ -> Nothing

eval :: Context -> Term -> Term
eval ctx t = case eval1 ctx t of
               Nothing -> t
               Just t' -> eval ctx t'

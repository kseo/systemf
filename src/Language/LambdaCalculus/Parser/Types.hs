module Language.LambdaCalculus.Parser.Types
  ( parseType
  ) where

import Data.List (elemIndex)

import Language.LambdaCalculus.Parser.Common
import Language.LambdaCalculus.Types

import Text.Parsec

parseTyVar :: LCParser Ty
parseTyVar = do
  v <- identifier
  list <- getState
  findTy v list

findTy :: String -> BoundContext -> LCParser Ty
findTy v list = case elemIndex v list of
  Nothing -> fail $ "The type variable " ++ v ++ " has not been bound"
  Just n  -> return $ TyVar n (length list)

parseTyAll :: LCParser Ty
parseTyAll = do
  reserved "All"
  x <- identifier
  modifyState (x :)
  _ <- dot
  ty <- parseType
  modifyState tail
  return $ TyAll x ty

parseTySome :: LCParser Ty
parseTySome = braces $ do
  reserved "Some"
  x <- identifier
  modifyState (x :)
  _ <- comma
  ty <- parseType
  modifyState tail
  return $ TySome x ty

parseTyBool :: LCParser Ty
parseTyBool = reserved "Bool" >> return TyBool

parseTyArr :: LCParser (Ty -> Ty -> Ty)
parseTyArr = arrow >> return TyArr

parseNonTyArr :: LCParser Ty
parseNonTyArr = parens parseType
            <|> parseTyBool
            <|> parseTyVar
            <|> parseTyAll
            <|> parseTySome

parseType :: LCParser Ty
parseType = parseNonTyArr `chainr1` parseTyArr

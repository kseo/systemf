{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Language.LambdaCalculus.Parser.Term
  ( parseLC
  ) where

import Data.List (elemIndex)

import Language.LambdaCalculus.AST
import Language.LambdaCalculus.Parser.Common
import Language.LambdaCalculus.Parser.Types

import Text.Parsec

parseTAbs :: LCParser Term
parseTAbs = do
  pos <- getPosition
  tlambda
  v <- identifier
  dot
  modifyState (v :)
  term <- parseTerm
  modifyState tail
  return $ TmTAbs (infoFrom pos) v term

parseAbs :: LCParser Term
parseAbs = do
  pos <- getPosition
  lambda
  v <- identifier
  colon
  ty <- parseType
  dot
  modifyState (v :)
  term <- parseTerm
  modifyState tail
  return $ TmAbs (infoFrom pos) v ty term

parseVar :: LCParser Term
parseVar = do
  v <- identifier
  list <- getState
  findVar v list

findVar :: String -> BoundContext -> LCParser Term
findVar v list = case elemIndex v list of
  Nothing -> fail $ "The variable " ++ v ++ " has not been bound"
  Just n  -> do
    pos <- getPosition
    return $ TmVar (infoFrom pos) n (length list)

parseTrue :: LCParser Term
parseTrue = do
  pos <- getPosition
  reserved "true"
  return $ TmTrue (infoFrom pos)

parseFalse :: LCParser Term
parseFalse = do
  pos <- getPosition
  reserved "false"
  return $ TmFalse (infoFrom pos)

parseIf :: LCParser Term
parseIf = do
  pos <- getPosition
  reserved "if"
  c <- parseTerm
  reserved "then"
  t <- parseTerm
  reserved "else"
  e <- parseTerm
  return $ TmIf (infoFrom pos) c t e

parsePack :: LCParser Term
parsePack = do
  pos <- getPosition
  (tyT1, tm) <- braces $ do
    star
    tyT1 <- parseType
    comma
    tm <- parseTerm
    return (tyT1, tm)
  reserved "as"
  tyT2 <- parseType
  return $ TmPack (infoFrom pos) tyT1 tm tyT2

parseUnpack :: LCParser Term
parseUnpack = do
  pos <- getPosition
  reserved "let"
  (tyX, x) <- braces $ do
    tyX <- identifier
    comma
    x <- identifier
    modifyState (tyX :)
    modifyState (x :)
    return (tyX, x)
  equals
  t1 <- parseTerm
  reserved "in"
  t2 <- parseTerm
  modifyState tail
  modifyState tail
  return $ TmUnpack (infoFrom pos) tyX x t1 t2

chainl1' :: LCParser a -> LCParser b -> LCParser (a -> b -> a) -> LCParser a
chainl1' p q op = do { x <- p; rest x }
  where rest x = do { f <- op; y <- q; rest (f x y) } <|> return x

parseNonApp :: LCParser Term
parseNonApp =  parens parseTerm   -- (M)
           <|> parseAbs           -- $\lambda$x.M
           <|> parseTAbs
           <|> parseIf            -- if a then b else c
           <|> parseTrue          -- true
           <|> parseFalse         -- false
           <|> parseVar           -- x
           <|> parsePack
           <|> parseUnpack

parseTerm :: LCParser Term
parseTerm = chainl1' parseNonApp parseNonAppOrTy $ do
  whiteSpace
  pos <- getPosition
  return (\x y -> case y of
    Left  z -> TmApp (infoFrom pos) x z
    Right z -> TmTApp (infoFrom pos) x z)
  where
    parseNonAppOrTy = Left <$> parseNonApp <|> Right <$> brackets parseType

parseWith :: Parsec String [u] a -> String -> Either ParseError a
parseWith p = runParser p [] "system f"

parseLC :: String -> Either ParseError Term
parseLC = parseWith (whiteSpace >> parseTerm)

-- |
-- The main module
--
module Language.LambdaCalculus
  ( module LC
  , run
  ) where

import Data.Bifunctor

import Language.LambdaCalculus.Context as LC
import Language.LambdaCalculus.Parser as LC
import Language.LambdaCalculus.Evaluator as LC
import Language.LambdaCalculus.Pretty as LC
import Language.LambdaCalculus.TypeChecker as LC
import Language.LambdaCalculus.Types as LC

-- |
-- Run the given script.
--
run :: String -> Either String String
run script = do
  term <- first show (parseLC script)
  _ <- first show (typeOf [] term)
  let value = eval [] term
  return (printTm [] value)

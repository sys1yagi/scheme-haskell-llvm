module Syntax where

import Control.Monad.Error
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
            | List [LispVal]
            | DottedList [LispVal] LispVal
            | Number Double
            | String String
            | Bool Bool

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String


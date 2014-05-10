{-# LANGUAGE OverloadedStrings #-}

module Emit where

import LLVM.General.Module
import LLVM.General.Context

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.FloatingPointPredicate as FP

import Data.Word
import Data.Int
import Control.Monad.Error
import Control.Applicative
import qualified Data.Map as Map

import Codegen
import qualified Syntax as S

toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (double, AST.Name x))

codegenTop :: S.LispVal -> LLVM ()
--codegenTop (S.Function name args body) = do
{-
codegenTop (List (Atom func : args)) = do
  --mapM eval args >>= apply func
  define double func fnargs bls
  where
    fnargs = toSig args
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM args $ \a -> do
        var <- alloca double
        store var (local (AST.Name a))
        assign a var
      cgen func args >>= ret
-}
--codegenTop (S.Extern name args) = do
--  external double name fnargs
--  where fnargs = toSig args

codegenTop exp = do
  define double "main" [] blks
  where
    blks = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      cgen exp >>= ret

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lt a b = do
  test <- fcmp FP.ULT a b
  uitofp double test

gen_primitives :: [(String, [AST.Operand] -> Codegen AST.Operand)]
gen_primitives = [("+", _numericBinop fadd),
              ("-", _numericBinop fsub),
              ("*", _numericBinop fmul),
              ("/", _numericBinop fdiv)]

_numericBinop :: (AST.Operand -> AST.Operand -> Codegen AST.Operand) -> [AST.Operand] -> Codegen AST.Operand
_numericBinop op singleVal@[a] = op a (_unpackNum $ S.Number 0)
_numericBinop op params = op (params !! 0 ) (params !! 1)


_unpackNum :: S.LispVal -> AST.Operand
_unpackNum (S.Number n) = cons_ $ C.Float (F.Double n)
_unpackNum (S.String n) = let parsed = reads n in 
                          if null parsed 
                            then cons_ $ C.Float (F.Double 0)
                            else cons_ $ C.Float (F.Double $ fst $ parsed !! 0)
_unpackNum (S.List [n]) = _unpackNum n
--_unpackNum notNum = throwError $ S.TypeMismatch "number" notNum 

cgen :: S.LispVal -> Codegen AST.Operand
--cgen (S.UnaryOp op a) = do
--  cgen $ S.Call ("unary" ++ op) [a]
{- 
  cgen (S.BinaryOp "=" (S.Var var) val) = do
  a <- getvar var
  cval <- cgen val
  store a cval
  return cval
-}
cgen (S.List (S.Atom func : args)) = do
  largs <- mapM cgen args
  maybe (return $ cons_ $ C.Float (F.Double 0))
                        ($ largs)
                        (lookup func gen_primitives)
  --Map.lookup func gen_primitives $ args
{-
    Just f  -> do
      ca <- cgen a
      cb <- cgen b
      f ca cb
    Nothing -> error "No such operator"
-}
--cgen (S.Var x) = getvar x >>= load
cgen (S.Number n) = return $ cons_ $ C.Float (F.Double n)
--cgen (S.String n) = return $ cons_ $ C.Float (F.Double n)
--cgen (S.Call fn args) = do
--  largs <- mapM cgen args
--  call (externf (AST.Name fn)) largs

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

liftError :: ErrorT String IO a -> IO a
liftError = runErrorT >=> either fail return

codegen :: AST.Module -> [S.LispVal] -> IO AST.Module
codegen mod fns = withContext $ \context ->
  liftError $ withModuleFromAST context newast $ \m -> do
    llstr <- moduleLLVMAssembly m
    putStrLn llstr
    return newast
  where
    modn    = mapM codegenTop fns
    newast  = runLLVM mod modn

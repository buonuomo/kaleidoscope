module Emit where

import Codegen
import qualified Syntax as S

import Control.Monad

import LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F


codegenTop :: S.Expr -> LLVM ()
codegenTop (S.Function name args body) = do
  definition double name fnargs blks
    where 
      fnargs = toIrArgs args
      blks = createBlocks $ execCodegen $ do
        entry <- addBlock entryBlockName
        setBlock entry
        forM args $ \arg -> do
          var <- alloca double
          store var (local (mkName arg))
          assign arg var
        cgen body >>= ret

codegenTop (S.Extern name args) = do
  extern double name (toIrArgs args)

codegenTop exp = do
  definition double "main" [] blks
  where
    blks = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      cgen exp >>= ret


toIrArgs :: [String] -> [(Type, Name)]
toIrArgs = map (\arg -> (double, mkName arg))

cgen :: S.Expr -> Codegen Operand

cgen (S.Float d) = pure . ConstantOperand . C.Float . F.Double $ d

cgen (S.BinOp S.Plus a b) = bind2 fadd (cgen a) (cgen b)
cgen (S.BinOp S.Minus a b) = bind2 fsub (cgen a) (cgen b)
cgen (S.BinOp S.Times a b) = bind2 fmul (cgen a) (cgen b)
cgen (S.BinOp S.Divide a b) = bind2 fdiv (cgen a) (cgen b)
cgen (S.BinOp S.LessThan a b) = bind2 flt (cgen a) (cgen b)

cgen (S.Var str) = getvar str >>= load

cgen (S.Call name args) = do
  args' <- mapM cgen args
  call (externf (length args) (mkName name)) args'

-- helper function for binary operators
bind2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bind2 f ma mb = do
  a <- ma
  b <- mb
  f a b

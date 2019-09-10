module Emit where

import Codegen
import qualified Syntax as S

import Control.Monad

import LLVM.AST as AST

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
          _ <- store var (local (mkName arg))
          assign arg var
        cgen body >>= ret


toIrArgs :: [String] -> [(Type, Name)]
toIrArgs = map (\arg -> (double, mkName arg))

cgen :: S.Expr -> Codegen Operand
cgen = undefined

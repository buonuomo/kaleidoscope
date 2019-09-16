module JIT where

import qualified Data.ByteString.Char8 as B

import qualified LLVM.AST as AST
import LLVM.Analysis
import LLVM.Context
import LLVM.Module
import LLVM.PassManager
import LLVM.Target

runJIT :: AST.Module -> IO AST.Module
runJIT mod =
  withContext $ \context -> do
    initializeAllTargets -- we need this for opt to work
    withModuleFromAST context mod $ \m ->
      withPassManager passes $ \pm -> do
        verify m
        runPassManager pm m
        optmod <- moduleAST m
        s <- moduleLLVMAssembly m
        B.putStrLn s
        return optmod

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }

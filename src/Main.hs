module Main where

import Codegen
import Emit
import Parser
import qualified Syntax as S

import Control.Monad.Trans
import qualified Data.ByteString.Char8 as B
import System.Console.Haskeline

import LLVM.Context
import LLVM.Module
import LLVM.AST as AST


main :: IO ()
main = runInputT defaultSettings (loop (emptyModule "main"))
  where
    loop :: AST.Module -> InputT IO ()
    loop mod = do
      input <- getInputLine "$ "
      case input of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input ->
          process input mod >>= loop 


process :: String -> AST.Module -> InputT IO AST.Module
process input mod =
  case parseProgram input of
    Right program ->
      liftIO $ codegen mod program
    Left err -> do
      outputStrLn . show $ err
      return mod



codegen :: AST.Module -> [S.Expr] -> IO AST.Module
codegen mod fns = withContext $ \context ->
  withModuleFromAST context newast $ \m -> do
    llstr <- moduleLLVMAssembly m
    B.putStrLn llstr
    return newast
  where
    modn = mapM codegenTop fns
    newast = runLLVM mod modn

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen where

import Control.Monad.State

import           Data.Map (Map)
import qualified Data.Map as Map

import           LLVM.AST as AST
import           LLVM.AST.Global
import           LLVM.AST.Linkage
import qualified LLVM.Prelude as LP

double :: Type
double = FloatingPointType DoubleFP

type SymbolTable = [(String, Operand)]

data CodegenState = CodegenState
  { currentBlock :: Name            -- name of active block to append to
  , blocks :: Map Name BlockState   -- Blocks for function
  , symtab :: SymbolTable           -- Function scope symbol table
  , blockCount :: Int               -- Count of basic blocks
  , count :: Word                   -- count of unnamed instructions
  , names :: [Name]                 -- name supply
  } deriving (Show)

data BlockState = BlockState
  { idx :: Int                        -- block index
  , stack :: [Named Instruction]      -- stack of instructions
  , term :: Maybe (Named Terminator)  -- block terminator
  } deriving (Show)

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState)

newtype LLVM a = LLVM (State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadState AST.Module)

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM mod (LLVM m) = execState m mod

emptyModule :: LP.ShortByteString -> AST.Module
emptyModule label = defaultModule { moduleName = label }

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }

definition :: Type -> LP.ShortByteString -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
definition retType label args body = addDefn $ 
  GlobalDefinition $ functionDefaults
    { returnType = retType
    , name = Name label 
    , parameters = (map (\(ty, nm) -> Parameter ty nm []) args, False)
    , basicBlocks = body
    }

extern :: Type -> LP.ShortByteString -> [(Type, Name)] -> LLVM ()
extern retType label args = addDefn $
  GlobalDefinition $ functionDefaults
    { linkage = External
    , returnType = retType
    , name = Name label
    , parameters = (map (\(ty, nm) -> Parameter ty nm []) args, False)
    , basicBlocks = []
    }

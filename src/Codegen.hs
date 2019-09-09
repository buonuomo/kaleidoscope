{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Codegen where

import Control.Monad.State

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.String

import           LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import           LLVM.AST.Global
import           LLVM.AST.Linkage
import qualified LLVM.Prelude as LP

double :: Type
double = FloatingPointType DoubleFP

type SymbolTable = [(String, Operand)]

type Names = Map String Int

data CodegenState = CodegenState
  { currentBlock :: Name            -- name of active block to append to
  , blocks :: Map Name BlockState   -- Blocks for function
  , symtab :: SymbolTable           -- Function scope symbol table
  , blockCount :: Int               -- Count of basic blocks
  , count :: Word                   -- count of unnamed instructions
  , names :: Names                  -- name supply
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

emptyModule :: String -> AST.Module
emptyModule label = defaultModule { moduleName = fromString label }

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }

definition :: Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
definition retType label args body = addDefn $ 
  GlobalDefinition $ functionDefaults
    { returnType = retType
    , name = mkName label 
    , parameters = (map (\(ty, nm) -> Parameter ty nm []) args, False)
    , basicBlocks = body
    }

extern :: Type -> String -> [(Type, Name)] -> LLVM ()
extern retType label args = addDefn $
  GlobalDefinition $ functionDefaults
    { linkage = External
    , returnType = retType
    , name = mkName label
    , parameters = (map (\(ty, nm) -> Parameter ty nm []) args, False)
    , basicBlocks = []
    }


-- Block manipulation

entry :: Codegen Name
entry = gets currentBlock

emptyBlock :: Int -> BlockState
emptyBlock n = BlockState
  { idx = n
  , stack = []
  , term = Nothing
  }

addBlock :: String -> Codegen Name
addBlock bname = do
  blks <- gets blocks
  n <- gets blockCount
  nms <- gets names
  let new = emptyBlock n
      (qname, supply) = uniqueName bname nms
  modify $ \s ->
    s { blocks = Map.insert (mkName qname) new blks
      , blockCount = n + 1
      , names = supply
      }
  return (mkName qname)

setBlock :: Name -> Codegen Name
setBlock bname = do
  modify $ \s -> s { currentBlock = bname }
  return bname

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  curr <- getBlock
  modify $ \s -> s { blocks = Map.insert curr new (blocks s) }

current :: Codegen BlockState
current = do
  currentName <- getBlock
  blks <- gets blocks
  case Map.lookup currentName blks of
    Nothing -> error $ "No such block: " ++ show currentName
    Just bs -> return bs

fresh :: Codegen Word
fresh = do
  idx <- gets count
  modify $ \s -> s { count = idx + 1 }
  return $ idx + 1

uniqueName :: String -> Names -> (String, Names)
uniqueName nm nms = 
  case Map.lookup nm nms of
    Nothing -> (nm, Map.insert nm 1 nms)
    Just i -> (nm ++ show i, Map.insert nm (i + 1) nms)

local :: Name -> Operand
local = LocalReference double

externf :: Name -> Operand
externf = ConstantOperand . C.GlobalReference double

assign :: String -> Operand -> Codegen ()
assign var x = do
  modify $ \s -> s { symtab = (var, x) : (symtab s) }

getvar :: String -> Codegen Operand
getvar var = do
  locals <- gets symtab
  case lookup var locals of
    Nothing -> error $ "Local variable not in scope: " ++ var
    Just op -> return op

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Codegen where

import Control.Monad.State

import           Data.Ord (comparing)
import           Data.List (sortBy)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.String

import           LLVM.AST as AST
import qualified LLVM.AST.AddrSpace as AS
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.FloatingPointPredicate as FPP
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

execCodegen :: Codegen a -> CodegenState
execCodegen codegen = execState (runCodegen codegen) emptyCodegenState

emptyCodegenState :: CodegenState
emptyCodegenState = CodegenState (mkName entryBlockName) Map.empty [] 1 0 Map.empty

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

-- | Create a list of basic blocks sorted by index from a codegen state
createBlocks :: CodegenState -> [BasicBlock]
createBlocks = map createBlock 
             . sortBy (comparing (idx . snd)) 
             . Map.toList 
             . blocks

createBlock :: (Name, BlockState) -> BasicBlock
createBlock (bname, BlockState{..}) = 
  case term of
    Nothing -> error $ "createBlock: missing terminator in block " ++ show bname
    Just t -> BasicBlock bname (reverse stack) t

entryBlockName :: String
entryBlockName = "entry"

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

-- external function with n arguments
externf :: Int -> Name -> Operand
externf n = ConstantOperand . C.GlobalReference (functionPtr n)

functionPtr :: Int -> Type
functionPtr n = PointerType
  { pointerReferent = functionType n
  , pointerAddrSpace = AS.AddrSpace 0
  }

functionType :: Int -> Type
functionType n = FunctionType
  { resultType = double
  , argumentTypes = replicate n double
  , isVarArg = False
  }

-- | assign a user defined name to an entry in the symbol table
assign :: String -> Operand -> Codegen ()
assign var x = do
  modify $ \s -> s { symtab = (var, x) : (symtab s) }

-- | get some entry from the symbol table
getvar :: String -> Codegen Operand
getvar var = do
  locals <- gets symtab
  case lookup var locals of
    Nothing -> error $ "Local variable not in scope: " ++ var
    Just op -> return op

-- | push an instruction onto the current block's stack and return a local
-- operator as a reference to this instruction
instr :: Instruction -> Codegen Operand
instr ins = do
  new <- fresh
  let name = UnName new
      named = name := ins
  curr <- current
  modifyBlock $ curr { stack = named : (stack curr) }
  return $ local name

-- | instructions of type void cannot be named, so we must treat them
-- differently
voidInstr :: Instruction -> Codegen ()
voidInstr ins = do
  curr <- current
  modifyBlock $ curr { stack = Do ins : (stack curr) }

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  curr <- current
  modifyBlock $ curr { term = Just trm }
  return trm

-- | binary operators
fadd :: Operand -> Operand -> Codegen Operand
fadd a b = instr $ FAdd noFastMathFlags a b []

fsub :: Operand -> Operand -> Codegen Operand
fsub a b = instr $ FSub noFastMathFlags a b []

fmul :: Operand -> Operand -> Codegen Operand
fmul a b = instr $ FMul noFastMathFlags a b []

fdiv :: Operand -> Operand -> Codegen Operand
fdiv a b = instr $ FDiv noFastMathFlags a b []

flt :: Operand -> Operand -> Codegen Operand
flt a b = instr (FCmp FPP.ULT a b []) >>= uitofp double

-- | terminators
br :: Name -> Codegen (Named Terminator)
br name = terminator $ Do $ Br name []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

-- | effectful instructions

-- | call a function with list of parameters
call :: Operand -> [Operand] -> Codegen Operand
call fun args = instr $ Call Nothing CC.C [] (Right fun) [(arg,[]) | arg <- args] [] []

-- | allocate some memory
alloca :: Type -> Codegen Operand
alloca ty = instr $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> Codegen ()
store ptr val = voidInstr $ Store False ptr val Nothing 0 []

load :: Operand -> Codegen Operand
load ptr = instr $ Load False ptr Nothing 0 []

-- | cast some int to a float type
uitofp :: Type -> Operand -> Codegen Operand
uitofp ty int = instr $ UIToFP int ty []

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen where

import Data.Word
import Data.String
import Data.List
import Data.Function
import qualified Data.Map as Map

import Control.Monad.State
import Control.Applicative

import LLVM.AST
import LLVM.AST.Global
import qualified LLVM.AST as AST

double :: Type
double = FloatingPointType 64 IEEE

type SymbolTable = [(String,Oprand)]

data CodegenState
    = CodegenState {
        currentBlock::Name
       ,blocks      ::Map.Map Name BlockState
       ,symtab      ::SymbolTable
       ,blockcount  ::Int
       ,count       ::Word
       ,names       ::Names
    }deriving Show
    
data BlockState
    =BlockState {
        idx   :: Int
      , stack :: [Named Instruction]
      , term  :: Maybe (Name Terminator)
    } deriving Show

newtype Codegen a = Codegen { runCodegen :: State CodegenState a} 
    deriving (Functor,Applicative,Monad,MonadState CodegenState)

newtype LLVM a = LLVM (State AST.Module)
-- |
-- Module      :  Data.Symbol.Unsafe
-- Copyright   :  (c) Harvard University 2009-2011
--             :  (c) Geoffrey Mainland 2011-2014
-- License     :  BSD-style
-- Maintainer  :  Geoffrey Mainland <mainland@cs.drexel.edu>
--
-- This module exposes the raw symbol constructor. Constructing symbols directly
-- can break the association between identifiers and strings. Prefer the abstract
-- API in "Data.Symbol" and construct symbols with 'intern'.

module Data.Symbol.Unsafe (
    Symbol(..),
    intern,
    unintern
  ) where

import           Control.Concurrent.MVar
import           Control.DeepSeq
import           Data.Data               (Constr, Data (..), DataType,
                                          Fixity (Prefix), constrIndex,
                                          mkConstr, mkDataType)
import qualified Data.Map                as Map
import           Data.String
import           System.IO.Unsafe        (unsafePerformIO)

-- | An interned string. Equality and ordering compare allocated identifiers in
-- constant time. Ordering can vary with evaluation order and between runs;
-- compare the results of 'unintern' for lexicographic ordering.
data Symbol =  -- | Unique identifier and the string itself
               Symbol {-# UNPACK #-} !Int !String

-- | Generic operations expose only the string and reconstruct through 'intern'
-- to preserve the association between the string and its unique identifier.
instance Data Symbol where
    gfoldl k z sym = z intern `k` unintern sym
    gunfold k z c
        | constrIndex c == 1 = k (z intern)
        | otherwise = error "Data.Symbol.Unsafe.gunfold: invalid constructor"
    toConstr _ = symbolConstr
    dataTypeOf _ = symbolDataType

symbolDataType :: DataType
symbolDataType = mkDataType "Data.Symbol.Unsafe.Symbol" [symbolConstr]

symbolConstr :: Constr
symbolConstr = mkConstr symbolDataType "Symbol" [] Prefix

instance Eq Symbol where
    (Symbol i1 _) == (Symbol i2 _) = i1 == i2

instance Ord Symbol where
    compare (Symbol i1 _) (Symbol i2 _) = compare i1 i2

instance Show Symbol where
    showsPrec d (Symbol _ s) = showsPrec d s

instance Read Symbol where
    readsPrec _ t = [(intern s, t') | (s, t') <- readList t]

instance IsString Symbol where
    fromString = intern

data SymbolEnv = SymbolEnv
    { uniq    :: {-# UNPACK #-} !Int
    , symbols :: !(Map.Map String Symbol)
    }

symbolEnv :: MVar SymbolEnv
{-# NOINLINE symbolEnv #-}
symbolEnv = unsafePerformIO $ newMVar $ SymbolEnv 1 Map.empty

-- We @'deepseq' s@ so that we can guarantee that when we perform the lookup we
-- won't potentially have to evaluate a thunk that might itself call @'intern'@,
-- leading to a deadlock.

-- | Intern a string using the synchronized global symbol table. Equal strings
-- produce equal symbols. The input is fully evaluated before accessing the
-- table, so it must be finite and fully defined.
--
-- Every distinct interned string and its symbol remain in the table for the
-- lifetime of the process, even after callers drop all references.
intern :: String -> Symbol
{-# NOINLINE intern #-}
intern s = s `deepseq` unsafePerformIO $ modifyMVar symbolEnv $ \env -> do
    case Map.lookup s (symbols env) of
      Nothing  -> do let sym  = Symbol (uniq env) s
                     let env' = env { uniq    = uniq env + 1,
                                      symbols = Map.insert s sym
                                                (symbols env)
                                    }
                     env' `seq` return (env', sym)
      Just sym -> return (env, sym)

-- | Return the string associated with a symbol.
unintern :: Symbol -> String
unintern (Symbol _ s) = s

-- Copyright (c) 2009-2012
--         The President and Fellows of Harvard College.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 3. Neither the name of the University nor the names of its contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.

-- THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE UNIVERSITY OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Symbol
-- Copyright   :  (c) Harvard University 2009-2012
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.Symbol (
    Symbol(..),
    intern,
    unintern
  ) where

import Control.Concurrent.MVar
import Data.Generics (Data, Typeable)
#if __GLASGOW_HASKELL__ >= 608
import Data.String
#endif /* __GLASGOW_HASKELL__ >= 608 */
import qualified Data.Map as Map
import System.IO.Unsafe (unsafePerformIO)

data Symbol =  -- | Unique identifier and the string itself
               Symbol {-# UNPACK #-} !Int !String
#if defined(__GLASGOW_HASKELL__)
  deriving (Data, Typeable)
#endif /* defined(__GLASGOW_HASKELL__) */

instance Eq Symbol where
    (Symbol i1 _) == (Symbol i2 _) = i1 == i2

instance Ord Symbol where
    compare (Symbol i1 _) (Symbol i2 _) = compare i1 i2

instance Show Symbol where
    showsPrec _ (Symbol _ s) = showString s

#if __GLASGOW_HASKELL__ >= 608
instance IsString Symbol where
    fromString = intern
#endif /* __GLASGOW_HASKELL__ >= 608 */

data SymbolEnv = SymbolEnv
    { uniq    :: {-# UNPACK #-} !Int
    , symbols :: !(Map.Map String Symbol)
    }

symbolEnv :: MVar SymbolEnv
{-# NOINLINE symbolEnv #-}
symbolEnv = unsafePerformIO $ newMVar $ SymbolEnv 1 Map.empty

-- We @seq@ @s@ so that we can guarantee that when we perform the lookup we
-- won't potentially have to evaluate a thunk that might itself call @intern@,
-- leading to a deadlock.

-- |Intern a string to produce a 'Symbol'.
intern :: String -> Symbol
{-# NOINLINE intern #-}
intern s = s `seq` unsafePerformIO $ modifyMVar symbolEnv $ \env -> do
    case Map.lookup s (symbols env) of
      Nothing  -> do let sym  = Symbol (uniq env) s
                     let env' = env { uniq    = uniq env + 1,
                                      symbols = Map.insert s sym
                                                (symbols env)
                                    }
                     env' `seq` return (env', sym)
      Just sym -> return (env, sym)

-- |Return the 'String' associated with a 'Symbol'.
unintern :: Symbol -> String
unintern (Symbol _ s) = s

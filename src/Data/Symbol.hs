{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :  Data.Symbol
-- Copyright   :  (c) Harvard University 2009-2011
--             :  (c) Geoffrey Mainland 2011-2014
-- License     :  BSD-style
-- Maintainer  :  Geoffrey Mainland <mainland@cs.drexel.edu>
--
-- Interned strings with constant-time equality and ordering. Ordering compares
-- allocated identifiers and can vary with evaluation order and between runs.
-- For lexicographic ordering, compare the strings returned by 'unintern'.
--
-- Every distinct interned string and its symbol remain in the global table for
-- the lifetime of the process, even when callers no longer reference them.
--
-- The @Data@ instance represents a symbol as a @Symbol@ constructor with a
-- single 'String' field. Generic construction and transformations use 'intern'
-- to preserve symbol identity. The internal identifier is not exposed.

module Data.Symbol (
    Symbol,
    intern,
    unintern
  ) where

import           Data.Symbol.Unsafe

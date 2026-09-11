{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :  Data.Symbol
-- Copyright   :  (c) Harvard University 2009-2011
--             :  (c) Geoffrey Mainland 2011-2014
-- License     :  BSD-style
-- Maintainer  :  Geoffrey Mainland <mainland@cs.drexel.edu>
--
-- Interned strings with constant-time equality and ordering. Use 'intern' to
-- create a symbol and 'unintern' to recover its string.
--
-- >>> intern "name" == intern "name"
-- True
-- >>> unintern (intern "name")
-- "name"
--
-- Symbols created through 'intern' compare equal exactly when their strings do.
-- Equality and ordering compare integer identifiers. Ordering follows
-- identifier allocation and can vary with evaluation order and between runs.
-- For lexicographic ordering, compare the strings returned by 'unintern'.
--
-- Interning uses a synchronized, process-wide table. Every distinct interned
-- string and its symbol remain in the table for the lifetime of the process,
-- even when callers no longer reference them. Memory use grows with the number
-- and total size of distinct strings interned. 'intern' fully evaluates its
-- input before accessing the table. Inputs must be finite and fully defined.
--
-- The @Data@ instance represents a symbol as a @Symbol@ constructor with a
-- single 'String' field. Generic construction and transformations use 'intern'
-- to preserve symbol identity. The internal identifier is not exposed.
--
-- This module provides the abstract API. "Data.Symbol.Unsafe" exposes the raw
-- constructor, which can break the association between identifiers and strings.

module Data.Symbol (
    Symbol,
    intern,
    unintern
  ) where

import           Data.Symbol.Unsafe

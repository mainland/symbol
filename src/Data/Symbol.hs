{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :  Data.Symbol
-- Copyright   :  (c) Harvard University 2009-2011
--             :  (c) Geoffrey Mainland 2011-2014
-- License     :  BSD-style
-- Maintainer  :  Geoffrey Mainland <mainland@cs.drexel.edu>
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

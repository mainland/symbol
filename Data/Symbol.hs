-- |
-- Module      :  Data.Symbol
-- Copyright   :  (c) Harvard University 2009-2011
--             :  (c) Geoffrey Mainland 2011-2013
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu

{-# LANGUAGE Trustworthy #-}

module Data.Symbol (
    Symbol,
    intern,
    unintern
  ) where

import Data.Symbol.Unsafe

{-# LANGUAGE MultiParamTypeClasses #-}

module Lambdabox.Translate
    ( Translate
    ) where

import Data.Text(Text)
import Lambdabox.Exchange

-- | Class for translating data types for specefic exchanges
-- Change into phantom type and GADT
class Exchange e => Translate e a b where
    translate :: e -> a -> b
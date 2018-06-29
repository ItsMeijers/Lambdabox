{-# LANGUAGE MultiParamTypeClasses #-}

module Lambdabox.Trade.Icebergable
    ( Icebergable(..)
    ) where

import Lambdabox.Box
import Lambdabox.Exchange
import Lambdabox.Symbol
import Lambdabox.Trade.Tradeable

class (Symbol a, Symbol b, Exchange e, Tradeable e a b) => Icebergable e a b

instance Icebergable Binance ADA BTC

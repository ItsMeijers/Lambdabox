{-# LANGUAGE MultiParamTypeClasses #-}

module Lambdabox.Trade.Icebergable
    ( Icebergable(..)
    ) where

import Lambdabox.Box
import Lambdabox.Exchange
import Lambdabox.Symbol
import Lambdabox.Trade.Tradeable

class (Tradeable e a b) => Icebergable e a b

instance Icebergable Binance ADA BTC

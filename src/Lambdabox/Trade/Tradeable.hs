{-# LANGUAGE MultiParamTypeClasses #-}

module Lambdabox.Trade.Tradeable
    ( Tradeable(..)
    ) where

import Lambdabox.Box
import Lambdabox.Exchange
import Lambdabox.Symbol

-- | The Tradeable class provides the possibilty of trading a pair of cryptos
-- on a specific exchange.
class (Symbol a, Symbol b, Exchange e) => Tradeable e a b

instance Tradeable Binance ADA BTC 
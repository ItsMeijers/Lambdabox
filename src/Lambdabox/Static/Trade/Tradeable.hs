{-# LANGUAGE MultiParamTypeClasses #-}

module Lambdabox.Static.Trade.Tradeable
    ( Tradeable(..)
    ) where

import Lambdabox.Types
import Lambdabox.Box
import Lambdabox.Static.Currency
import Lambdabox.Static.Exchange
import Lambdabox.Static.Trade.ExchangeSymbol

-- | The Tradeable class provides the possibilty of trading a pair of cryptos
-- on a specific exchange.
class (Currency a, Currency b, Exchange e) => Tradeable e a b where
    executeTrade     :: ExchangeSymbol e a b -> Order -> Box OrderResponse
    executeTestTrade :: ExchangeSymbol e a b -> Order -> Box ()

instance Tradeable Binance BTC ADA where
    executeTrade     = undefined
    executeTestTrade = undefined
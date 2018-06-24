{-# LANGUAGE MultiParamTypeClasses #-}

module Lambdabox.Static.Trade.Icebergable
    ( Icebergable(..)
    ) where

import Lambdabox.Box
import Lambdabox.Types
import Lambdabox.Static.Exchange
import Lambdabox.Static.Currency
import Lambdabox.Static.Exchange
import Lambdabox.Static.Trade.Tradeable
import Lambdabox.Static.Trade.ExchangeSymbol

class (Currency a, Currency b, Exchange e, Tradeable e a b) => Icebergable e a b where
    executeIceberg     :: ExchangeSymbol e a b -> Order -> Double -> Box OrderResponse
    executeTestIceberg :: ExchangeSymbol e a b -> Order -> Double -> Box ()

instance Icebergable Binance BTC ADA where
    executeIceberg     = undefined
    executeTestIceberg = undefined

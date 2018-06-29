{-# LANGUAGE MultiParamTypeClasses #-}

module Lambdabox.Trade.Order
    ( Order(..)
    ) where

import Lambdabox.Translate
import Lambdabox.Exchange
import Binance.Internal.Trade.Types (BinanceOrder)

data Order = Order { test :: String }

instance Translate Binance Order BinanceOrder where
    translate = undefined
module Lambdabox.Trade.Order
    (

    ) where

data Order = Order { test :: String }

instance Translate Binance Order BinanceOrder
    translate = undefined
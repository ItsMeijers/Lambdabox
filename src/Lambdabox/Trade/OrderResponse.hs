{-# LANGUAGE MultiParamTypeClasses #-}

module Lambdabox.Trade.OrderResponse where

import Lambdabox.Translate
import Lambdabox.Exchange
import Binance.Internal.Trade.Types (BinanceOrderResponse)

data OrderResponse = OrderResponse { r :: String }

-- | Translates the underlying Binance Order Response to the geneeral 
-- Order Response.
instance Translate Binance BinanceOrderResponse OrderResponse where
    translate = undefined
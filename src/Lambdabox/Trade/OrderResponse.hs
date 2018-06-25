module Lambdabox.Trade.OrderResponse where

data OrderResponse = OrderResponse { r :: String }

-- | Translates the underlying Binance Order Response to the geneeral 
-- Order Response.
instance Translate Binance BinanceOrderResponse OrderResponse where
    Translate = undefined
{-# LANGUAGE MultiParamTypeClasses #-}

module Lambdabox.Trade.Order
    ( Order(..)
    ) where

import Lambdabox.Translate
import Lambdabox.Exchange
import Binance.Internal.Trade.Types
import Lambdabox.Trade.TimeInForce

data Order = LimitOrder 
                { timeInForce :: !TimeInForce
                , quantity    :: !Double
                , price       :: !Double
                }
           | MarketOrder 
                { quantity    :: !Double 
                }
           | StopLossOrder
                { quantity    :: !Double
                , stopPrice   :: !Double 
                }
           | StopLossLimitOrder
                { quantity    :: !Double
                , stopPrice   :: !Double
                , timeInForce :: !TimeInForce
                , price       :: !Double
                }
           | TakeProfit
                { quantity    :: !Double
                , stopPrice   :: !Double
                }
           | TakeProfitLimit
                { timeInForce :: !TimeInForce
                , quantity    :: !Double
                , price       :: !Double
                , stopPrice   :: !Double
                }
           | LimitMaker
                { quantity    :: !Double
                , price       :: !Double
                }
            deriving (Show, Eq)

instance Translate Binance Order BinanceOrder where
    translate b (MarketOrder q)                = BinanceMarketOrder q
    translate b (StopLossOrder qty sp)         = BinanceStopLossOrder qty sp
    translate b (TakeProfit qty sp)            = BinanceTakeProfit qty sp
    translate b (LimitMaker qty p)             = BinanceLimitMaker qty p
    translate b (LimitOrder tif qty p)         = 
        BinanceLimitOrder (translate b tif) qty p Nothing
    translate b (TakeProfitLimit tif qty p sp) = 
        BinanceTakeProfitLimit (translate b tif) qty p sp Nothing
module Lambdabox.Trade
    ( ExchangePairSide(..)
    , Tradeable(..)
    , Icebergable(..)
    , trade
    , testTrade
    , iceberg
    , testIceberg
    , (//)
    , on
    ) where

import Lambdabox.Box
import Lambdabox.Types
import Lambdabox.Exchange
import Lambdabox.ExchangePair
import Lambdabox.Trade.Tradeable
import Lambdabox.Trade.Icebergable
import Lambdabox.Trade.Side

-- | Execute a trade based on a Symbol, an Exchange and an Order
-- Example:
-- let symbol = BTC `for` ETH `on` Binance
--     order  = LimitOrder GoodTilCanceled 20000 0.13
-- in trade symbol order
trade :: Tradeable e a b => ExchangePairSide e a b -> Order -> Box OrderResponse
trade es = undefined

-- | Test a trade based on a Symbol, an Exchange and an Order
testTrade :: Tradeable e a b => ExchangePairSide e a b -> Order -> Box ()
testTrade es = undefined

-- | Execute a trade in an icerberg fashion based on a Symbol, an Exchange and
-- an Order. Note that not every exchange supports iceberging.
-- Example:
-- let order = LimitOrder GoodTilCanceled 20000 0.13
--     pair  = ADA `for` BTC `on` Binance 
-- in iceberg pair order 1000
iceberg :: Icebergable e a b
        => ExchangePairSide e a b
        -> Order
        -> Double
        -> Box OrderResponse
iceberg es o = undefined

-- | Test an iceberging trade based on a Symbol, an Exchange and an Order
testIceberg :: Icebergable e a b
            => ExchangePairSide e a b
            -> Order
            -> Double
            -> Box ()
testIceberg es o = undefined
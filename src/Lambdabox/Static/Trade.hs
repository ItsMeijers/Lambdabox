module Lambdabox.Static.Trade
    ( ExchangeSymbol(..)
    , Tradeable(..)
    , Icebergable(..)
    , trade
    , testTrade
    , iceberg
    , testIceberg
    , for
    , on
    ) where

import Lambdabox.Box
import Lambdabox.Types
import Lambdabox.Static.Currency
import Lambdabox.Static.Exchange
import Lambdabox.Static.Trade.Tradeable
import Lambdabox.Static.Trade.Icebergable
import Lambdabox.Static.Trade.ExchangeSymbol

-- | Execute a trade based on a Symbol, an Exchange and an Order
-- Example:
-- let symbol = BTC `for` ETH `on` Binance
--     order  = LimitOrder GoodTilCanceled 20000 0.13
-- in trade symbol order
trade :: Tradeable e a b => ExchangeSymbol e a b -> Order -> Box OrderResponse
trade es = executeTrade es

-- | Test a trade based on a Symbol, an Exchange and an Order
testTrade :: Tradeable e a b => ExchangeSymbol e a b -> Order -> Box ()
testTrade es = executeTestTrade es

-- | Execute a trade in an icerberg fashion based on a Symbol, an Exchange and
-- an Order. Note that not every exchange supports iceberging.
-- Example:
-- let order = LimitOrder GoodTilCanceled 20000 0.13
--     pair  = ADA `for` BTC `on` Binance 
-- in iceberg pair order 1000
iceberg :: Icebergable e a b
        => ExchangeSymbol e a b
        -> Order
        -> Double
        -> Box OrderResponse
iceberg es o = executeIceberg es o

-- | Test an iceberging trade based on a Symbol, an Exchange and an Order
testIceberg :: Icebergable e a b
            => ExchangeSymbol e a b
            -> Order
            -> Double
            -> Box ()
testIceberg es o = executeTestIceberg es o
module Lambdabox.Trade
    ( ExchangePairSide(..)
    , Tradeable(..)
    , Icebergable(..)
    , Order(..)
    , trade
    , testTrade
    , iceberg
    , testIceberg
    , (//)
    , on
    , buy
    , sell
    ) where

import           Lambdabox.Box
import           Lambdabox.Symbol
import           Lambdabox.Exchange
import           Lambdabox.ExchangePair
import           Lambdabox.Translate
import           Lambdabox.Trade.Side
import           Lambdabox.Trade.Tradeable
import           Lambdabox.Trade.Icebergable
import           Lambdabox.Trade.Side
import           Lambdabox.Trade.Order
import           Lambdabox.Trade.OrderResponse
import           Data.Text (concat)
import           System.IO.Error (userError, ioError)
import           Control.Monad.Except (liftIO)
import qualified Binance.Internal.Trade.Http as BI

-- | Execute a trade based on a Symbol, an Exchange and an Order
-- Example:
-- let symbol = buy BTC//ETH `on` Binance
--     order  = LimitOrder GoodTilCanceled 20000 0.13
-- in trade symbol order
trade :: (Tradeable e a b)
      => ExchangePairSide e a b
      -> Order
      -> Box OrderResponse
trade (ExchangePair e a b, s) o 
    | isBinance e = tradeBinanceOrder a b s o
    | otherwise   = liftIO $ ioError $ userError "Not implemented Exchange found!"

tradeBinanceOrder :: (Symbol a, Symbol b)
                  => a
                  -> b
                  -> Side
                  -> Order
                  -> Box OrderResponse
tradeBinanceOrder a b side order = do
    let symbol   = translate Binance (a, b)
        bSide    = translate Binance side
        bOrder   = translate Binance order
    response     <- BI.trade symbol bSide bOrder Nothing Nothing Nothing -- TODO add a way to incorperate the recvWindow etc
    return $ translate Binance response

-- | Test a trade based on a Symbol, an Exchange and an Order
testTrade :: Tradeable e a b => ExchangePairSide e a b -> Order -> Box ()
testTrade (ExchangePair e a b, s) o 
    | isBinance e = testTradeBinanceOrder a b s o
    | otherwise   = liftIO $ ioError $ userError "Not implemented Exchange found!"

testTradeBinanceOrder :: (Symbol a, Symbol b)
                      => a
                      -> b
                      -> Side
                      -> Order
                      -> Box ()
testTradeBinanceOrder a b side order = do
    let symbol   = translate Binance (a, b)
        bSide    = translate Binance side
        bOrder   = translate Binance order
    BI.testTrade symbol bSide bOrder Nothing Nothing Nothing -- TODO add a way to incorperate the recvWindow etc
    

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
iceberg (ExchangePair e a b, s) o icebergQuantity
    | isBinance e = icebergBinanceOrder a b s o icebergQuantity
    | otherwise   = liftIO $ ioError $ userError "Not implemented Exchange found!"


icebergBinanceOrder :: (Symbol a, Symbol b)
                    => a
                    -> b
                    -> Side
                    -> Order
                    -> Double
                    -> Box OrderResponse
icebergBinanceOrder a b side order iceQty = do
    let symbol   = translate Binance (a, b)
        bSide    = translate Binance side
        bOrder   = translate Binance (IcebergOrder order iceQty)
    response     <- BI.trade symbol bSide bOrder Nothing Nothing Nothing -- TODO add a way to incorperate the recvWindow etc
    return $ translate Binance response


-- | Test an iceberging trade based on a Symbol, an Exchange and an Order
testIceberg :: Icebergable e a b
            => ExchangePairSide e a b
            -> Order
            -> Double
            -> Box ()
testIceberg (ExchangePair e a b, s) o icebergQuantity
    | isBinance e = testIcebergBinanceOrder a b s o icebergQuantity
    | otherwise   = liftIO $ ioError $ userError "Not implemented Exchange found!"

testIcebergBinanceOrder :: (Symbol a, Symbol b)
                        => a
                        -> b
                        -> Side
                        -> Order
                        -> Double
                        -> Box ()
testIcebergBinanceOrder a b side order iceQty = do
    let symbol   = translate Binance (a, b)
        bSide    = translate Binance side
        bOrder   = translate Binance (IcebergOrder order iceQty)
    BI.testTrade symbol bSide bOrder Nothing Nothing Nothing 
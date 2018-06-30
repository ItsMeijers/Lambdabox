{-# LANGUAGE OverloadedStrings #-}

module Binance.Internal.MarketData.Http
    ( orderBook
    , recentTrades
    , historicalTrades
    , aggregatedTrades
    , candlestickData
    , dayPriceChangeStatistics
    , dayPriceChangeStatisticsFor
    , latestPrice
    , latestPriceFor
    , orderBookTicker
    , orderBookTickerFor
    ) where

import Lambdabox.Box
import Binance.Internal.MarketData.Types
import Network.Wreq.Extended
import Data.Text (Text)

orderBook :: Text -> Maybe Int -> Box OrderBook
orderBook s limit = get "/api/v1/depth" $ [("symbol", s)]  
                                ++ optionalParams ["limit" :? limit]

recentTrades :: Text -> Maybe Int -> Box [Trade]
recentTrades s limit = get "/api/v1/trades" $ [("symbol", s)]
                                ++ optionalParams ["limit" :? limit]

historicalTrades :: Text -> Maybe Int -> Maybe Int -> Box [Trade]
historicalTrades s limit fromId = get "/api/v1/historicalTrades" $ 
                                        [("symbol", s)] ++ optionalParams 
                                        ["limit" :? limit, "fromId" :? fromId]

aggregatedTrades :: Text 
                    -> Maybe Int 
                    -> Maybe Int 
                    -> Maybe Int 
                    -> Maybe Int 
                    -> Box [AggregatedTrade]
aggregatedTrades symbol fromId startTime endTime limit =
    get "/api/v1/aggTrades" $ [("symbol", symbol)] ++ optionalParams 
        [ "fromId"    :? fromId
        , "startTime" :? startTime
        , "endTime"   :? endTime
        , "limit"     :? limit ]

candlestickData :: Text 
                -> Interval
                -> Maybe Int
                -> Maybe Int
                -> Maybe Int
                -> Box [Candlestick]
candlestickData symbol interval limit startTime endTime =
    get "/api/v1/klines" $ 
        [("symbol", symbol), ("interval", toText interval)] ++
        optionalParams [ "limit"     :? limit
                        , "startTime" :? startTime
                        , "endTime"   :? endTime ]

dayPriceChangeStatistics :: Box [DayPriceChange]
dayPriceChangeStatistics = get "/api/v1/ticker/24hr" []

dayPriceChangeStatisticsFor :: Text -> Box DayPriceChange
dayPriceChangeStatisticsFor symbol = 
    get "/api/v1/ticker/24hr" [("symbol", symbol)]

latestPrice :: Box [LatestPrice]
latestPrice = get "/api/v3/ticker/price" []

latestPriceFor :: Text -> Box LatestPrice
latestPriceFor symbol = get "/api/v3/ticker/price" [("symbol", symbol)]

orderBookTicker :: Box [OrderBookTicker]
orderBookTicker = get "/api/v3/ticker/bookTicker" []

orderBookTickerFor :: Text -> Box OrderBookTicker
orderBookTickerFor symbol = get "/api/v3/ticker/bookTicker" 
                                    [("symbol", symbol)]

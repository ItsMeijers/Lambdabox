
module Binance.Internal.MarketData.Stream
    (

    ) where

import Binance.Internal.MarketData.Types (Interval)

-- data BinanceStream a = BinanceStream a [StreamArguments]

-- combine :: BinanceStream a -> BinanceStream b -> BinanceStream (a, b)
-- combine = undefined

-- -- For each stream create a channel -> send the output of the websocket to each
-- -- channel! Thereby passing the message asynchrounsly to some channel where 
-- -- the user can listen for new messages asynchrnously :)
-- runBinanceStream :: BinanceStream a -> IO a
-- runBinanceStream = undefined

-- -- TODO Create StreamArguments for each possible function below? So that
-- -- each function crates a instanc eo fthis that can be run by calling
-- -- runBinanceStream? 
-- data StreamArguments = StreamArguments
--         { name     :: Text 
--         , symbol   :: !(Maybe Text)
--         , interval :: !(Maybe Text)
--         , allData  :: Bool
--         , level    :: !(Maybe Level)
--         }

-- showStreamArguments :: StreamArguments -> Text
-- showStreamArguments = undefined

-- aggregatedTrades :: Symbol -> ???
-- aggregatedTrades = undefined

-- trades :: Symbol -> ???
-- trades = undefined

-- candlesticks :: Symbol -> Interval -> ???
-- candlesticks = undefined

-- -- | 24 hr statistics for a specific symbol pushed in an array each second
-- dayTicker :: Symbol -> ???
-- dayTicker = undefined

-- -- | 24 hr statistics for all symbols pushed in an array each second
-- dayTickerAll :: ???
-- dayTickerAll = undefined

-- -- | Top 'levels' bids and asks, pushed every second
-- partialBookDepths :: Symbol -> Level -> ???
-- partialBookDepths = undefined

-- -- | Order book price and quanity depth updates used to locally manage an order 
-- -- book. Pushed every second. Use this function to create a local order book 
-- -- abstraction!  
-- diffDepths :: Symbol -> ???
-- diffDepths = undefined
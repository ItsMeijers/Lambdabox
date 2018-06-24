{-# LANGUAGE OverloadedStrings #-}

module Binance.Internal.Exchange.Http
    ( ping
    , binanceTime
    , exchangeInfo
    ) where

import Network.Wreq.Extended
import Binance.Internal.Types
import Binance.Internal.Exchange.Types
import Data.Aeson.Extended (Unit)

-- | Pings the Binance server to check wether a connection can be made
ping :: Binance ()
ping = do
    _ <- get "/api/v1/ping" [] :: Binance Unit
    return ()

-- | Retrieves the time from the Binance server
binanceTime :: Binance ServerTime
binanceTime = get "/api/v1/time" []

-- | Retrieves the echange info of Binance
exchangeInfo :: Binance ExchangeInfo
exchangeInfo = get "/api/v1/exchangeInfo" []
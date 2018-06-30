{-# LANGUAGE OverloadedStrings #-}

module Binance.Internal.Exchange.Http
    ( ping
    , binanceTime
    , exchangeInfo
    ) where

import Lambdabox.Box
import Network.Wreq.Extended
import Binance.Internal.Exchange.Types
import Data.Aeson.Extended (Unit)

-- | Pings the Binance server to check wether a connection can be made
ping :: Box ()
ping = do
    _ <- get "/api/v1/ping" [] :: Box Unit
    return ()

-- | Retrieves the time from the Binance server
binanceTime :: Box ServerTime
binanceTime = get "/api/v1/time" []

-- | Retrieves the echange info of Binance
exchangeInfo :: Box ExchangeInfo
exchangeInfo = get "/api/v1/exchangeInfo" []
{-# LANGUAGE OverloadedStrings #-}

module Binance.Internal.Account.Stream.Http
    ( start
    , keepAlive
    , close
    ) where

import Binance.Internal.Types
import Binance.Internal.Account.Stream.Types
import Network.Wreq.Extended
import Data.Text (Text)

start :: Binance DataStream
start = post "/api/v1/userDataStream" []

keepAlive :: Text -> Binance ()
keepAlive lk = put "/api/v1/userDataStream" [("listenKey", lk)]

close :: Text -> Binance ()
close lk = delete "/api/v1/userDataStream" [("listenKey", lk)]
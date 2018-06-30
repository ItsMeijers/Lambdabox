{-# LANGUAGE OverloadedStrings #-}

module Binance.Internal.Account.Stream.Http
    ( start
    , keepAlive
    , close
    ) where

import Lambdabox.Box
import Binance.Internal.Account.Stream.Types
import Network.Wreq.Extended
import Data.Text (Text)

start :: Box DataStream
start = post "/api/v1/userDataStream" []

keepAlive :: Text -> Box ()
keepAlive lk = put "/api/v1/userDataStream" [("listenKey", lk)]

close :: Text -> Box ()
close lk = delete "/api/v1/userDataStream" [("listenKey", lk)]
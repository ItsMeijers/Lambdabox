{-# LANGUAGE OverloadedStrings #-}

module Binance.Internal.Account.Http
    ( accountInfo
    , accountTrades
    ) where

import Lambdabox.Box
import Binance.Internal.Account.Types
import Network.Wreq.Extended
import Data.Text (Text)

accountInfo :: Maybe Int -> Box Account
accountInfo recvWindow = getSigned "/api/v3/account" $ optionalParams 
                                        ["recvWindow" :? recvWindow]

accountTrades :: Text 
                -> Maybe Int 
                -> Maybe Int 
                -> Maybe Int 
                -> Box [AccountTrade]
accountTrades symbol limit fromId recvWindow =
    getSigned "/api/v3/myTrades" $ [("symbol", symbol)] ++ optionalParams 
        ["limit" :? limit, "fromId" :? fromId, "recvWindow" :? recvWindow]
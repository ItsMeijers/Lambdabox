{-# LANGUAGE OverloadedStrings #-}

module Binance.Internal.Account.Http
    ( accountInfo
    , accountTrades
    ) where

import Binance.Internal.Types
import Binance.Internal.Account.Types
import Network.Wreq.Extended
import Data.Text (Text)

accountInfo :: Maybe Int -> Binance Account
accountInfo recvWindow = getSigned "/api/v3/account" $ optionalParams 
                                        ["recvWindow" :? recvWindow]

accountTrades :: Text 
                -> Maybe Int 
                -> Maybe Int 
                -> Maybe Int 
                -> Binance [AccountTrade]
accountTrades symbol limit fromId recvWindow =
    getSigned "/api/v3/myTrades" $ [("symbol", symbol)] ++ optionalParams 
        ["limit" :? limit, "fromId" :? fromId, "recvWindow" :? recvWindow]
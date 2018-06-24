{-# LANGUAGE DeriveGeneric #-}

module Binance.Internal.Account.Types
    ( Account(..)
    , Balance(..)
    , AccountTrade(..)
    ) where

import GHC.Generics
import Data.Aeson.Extended
import Data.Text (Text)

data Account = Account
    { makerCommission  :: !Int
    , takerCommission  :: !Int
    , buyerCommission  :: !Int
    , sellerCommission :: !Int
    , canTrade         :: !Bool
    , canWithdraw      :: !Bool
    , canDeposit       :: !Bool
    , updateTime       :: !Int
    , balances         :: ![Balance]
    } deriving (Show, Eq, Generic)

instance FromJSON Account

data Balance = Balance
    { asset  :: !Text
    , free   :: !Text
    , locked :: !Text
    } deriving (Show, Eq, Generic)

instance FromJSON Balance

data AccountTrade = AccountTrade
    { id          :: !Int
    , orderId     :: !Int
    , price       :: !Text
    , qty         :: !Text
    , commission  :: !Text
    , time        :: !Int
    , isBuyer     :: !Bool
    , isMaker     :: !Bool
    , isBestMatch :: !Bool
    } deriving (Show, Eq, Generic)

instance FromJSON AccountTrade
{-# LANGUAGE DeriveGeneric, DuplicateRecordFields #-}

module Binance.Internal.Account.Stream.Types
    ( DataStream(..)
    ) where

import GHC.Generics
import Data.Aeson.Extended
import Data.Text (Text)

-- | Model that is returned for opening a data stream from Binance
data DataStream = DataStream
    { listenKey :: !Text
    } deriving (Show, Eq, Generic)

instance FromJSON DataStream
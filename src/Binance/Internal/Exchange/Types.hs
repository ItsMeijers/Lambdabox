{-# LANGUAGE DeriveGeneric, DuplicateRecordFields #-}

module Binance.Internal.Exchange.Types
    ( ServerTime(..)
    , ExchangeInfo(..)
    , RateLimit(..)
    , Symbol(..)
    , Filter(..)    
    ) where

import GHC.Generics
import Data.Aeson.Extended
import Data.Text (Text)

data ServerTime = ServerTime
    { serverTime :: Int
    } deriving (Show, Eq, Generic)

instance FromJSON ServerTime

data ExchangeInfo = ExchangeInfo
    { timezone        :: !Text
    , serverTime      :: !Int
    , rateLimits      :: ![RateLimit]
    -- TODO ADD !, exchangeFilters :: ![ExchangeFilter]
    , symbols         :: ![Symbol]
    } deriving (Show, Eq, Generic)

instance FromJSON ExchangeInfo

data RateLimit = RateLimit
    { rateLimitType :: !Text
    , interval      :: !Text
    , limit         :: !Int
    } deriving (Show, Eq, Generic)

instance FromJSON RateLimit

data Symbol = Symbol
    { symbol             :: !Text
    , status             :: !Text
    , baseAsset          :: !Text
    , baseAssetPrecision :: !Int
    , quoteAsset         :: !Text
    , quotePrecision     :: !Int
    , orderTypes         :: ![Text]
    , icebergAllowed     :: !Bool
    , filters            :: ![Filter]
    } deriving (Show, Eq, Generic)

instance FromJSON Symbol

data Filter = Filter
    { filterType  :: !Text
    , minPrice    :: !(Maybe Text)
    , maxPrice    :: !(Maybe Text)
    , tickSize    :: !(Maybe Text)
    , minQty      :: !(Maybe Text)
    , maxQty      :: !(Maybe Text)
    , stepSize    :: !(Maybe Text)
    , minNotional :: !(Maybe Text)
    } deriving (Show, Eq, Generic)

instance FromJSON Filter
{-# LANGUAGE DeriveGeneric, DuplicateRecordFields, OverloadedStrings #-}

module Binance.Internal.MarketData.Types
    ( OrderBook(..)
    , PriceQuantity(..)
    , Trade(..)
    , AggregatedTrade(..)
    , Interval(..)
    , Candlestick(..)
    , DayPriceChange(..)
    , LatestPrice(..)
    , OrderBookTicker(..)
    ) where

import GHC.Generics
import Data.Aeson.Extended
import Data.Text (Text)
import Data.Vector((!))

data OrderBook = OrderBook
    { lastUpdateId :: !Int
    , bids         :: ![PriceQuantity]
    , asks         :: ![PriceQuantity]
    } deriving (Show, Eq, Generic)

instance FromJSON OrderBook

data PriceQuantity = PriceQuantity
    { price    :: !Double
    , quantity :: !Double
    } deriving (Show, Eq)

instance FromJSON PriceQuantity where
    parseJSON (Array xs) = do
        p <- parseJSON $ xs ! 0
        q <- parseJSON $ xs ! 1
        return $ PriceQuantity {price = read p, quantity = read q}

data Trade = Trade
    { id           :: !Int
    , price        :: !Text
    , qty          :: !Text
    , time         :: !Int
    , isBuyerMaker :: !Bool
    , isBestMatch  :: !Bool
    } deriving (Show, Eq, Generic)

instance FromJSON Trade

data AggregatedTrade = AggregatedTrade
    { aggrTradeId  :: !Int
    , price        :: !Text
    , qty          :: !Text
    , fstTradeId   :: !Int
    , lstTradeId   :: !Int
    , timestamp    :: !Int
    , isBuyerMaker :: !Bool
    , isBestMatch  :: !Bool
    } deriving (Show, Eq)

instance FromJSON AggregatedTrade where
    parseJSON (Object v) =
        AggregatedTrade <$> v .: "a"
                        <*> v .: "p"
                        <*> v .: "q"
                        <*> v .: "f"
                        <*> v .: "l"
                        <*> v .: "T"
                        <*> v .: "m"
                        <*> v .: "M"

data Interval = OneMinute
              | ThreeMinutes
              | FiveMinutes
              | FifteenMinutes
              | ThirtyMinutes
              | OneHour
              | TwoHours
              | FourHours
              | SixHours
              | EightHours
              | TwelveHours
              | OneDay
              | ThreeDays
              | OneWeek
              | OneMonth

instance Show Interval where
    show OneMinute      = "1m"  
    show ThreeMinutes   = "3m" 
    show FiveMinutes    = "5m"
    show FifteenMinutes = "15m" 
    show ThirtyMinutes  = "30m"
    show OneHour        = "1h"
    show TwoHours       = "2h"
    show FourHours      = "4h"
    show SixHours       = "6h"
    show EightHours     = "8h"
    show TwelveHours    = "12h"
    show OneDay         = "1d"
    show ThreeDays      = "3d" 
    show OneWeek        = "1w"
    show OneMonth       = "1M"

data Candlestick = Candlestick
    { openTime                 :: !Int
    , open                     :: !String
    , high                     :: !String
    , low                      :: !String
    , close                    :: !String
    , volume                   :: !String
    , closeTime                :: !Int
    , quoteAssetVolume         :: !String
    , numberOfTrades           :: !Int
    , takerBuyAssetVolume      :: !String
    , takerBuyQuoteAssetVolume :: !String
    } deriving (Show, Eq)

instance FromJSON Candlestick where
    parseJSON (Array xs) = do
        openTime                 <- parseJSON $ xs ! 0
        open                     <- parseJSON $ xs ! 1
        high                     <- parseJSON $ xs ! 2
        low                      <- parseJSON $ xs ! 3 
        close                    <- parseJSON $ xs ! 4
        volume                   <- parseJSON $ xs ! 5
        closeTime                <- parseJSON $ xs ! 6
        quoteAssetVolume         <- parseJSON $ xs ! 7
        numberOfTrades           <- parseJSON $ xs ! 8
        takerBuyAssetVolume      <- parseJSON $ xs ! 9
        takerBuyQuoteAssetVolume <- parseJSON $ xs ! 10
        return Candlestick 
                    { openTime                 = openTime
                    , open                     = open
                    , high                     = high
                    , low                      = low
                    , close                    = close
                    , volume                   = volume
                    , closeTime                = closeTime
                    , quoteAssetVolume         = quoteAssetVolume
                    , numberOfTrades           = numberOfTrades
                    , takerBuyAssetVolume      = takerBuyAssetVolume
                    , takerBuyQuoteAssetVolume = takerBuyQuoteAssetVolume }

data DayPriceChange = DayPriceChange
    { symbol             :: !Text
    , priceChange        :: !Text
    , priceChangePercent :: !Text
    , weightedAvgPrice   :: !Text
    , prevClosePrice     :: !Text
    , lastPrice          :: !Text
    , lastQty            :: !Text
    , bidPrice           :: !Text
    , askPrice           :: !Text
    , openPrice          :: !Text
    , highPrice          :: !Text
    , lowPrice           :: !Text
    , volume             :: !Text
    , quoteVolume        :: !Text
    , openTime           :: !Int
    , closeTime          :: !Int
    , firstId            :: !Int
    , lastId             :: !Int
    , count              :: !Int
    } deriving (Show, Eq, Generic)

instance FromJSON DayPriceChange

data LatestPrice = LatestPrice
    { symbol :: !Text
    , price  :: !Text
    } deriving (Show, Eq, Generic)

instance FromJSON LatestPrice

data OrderBookTicker = OrderBookTicker
    { symbol   :: !Text
    , bidPrice :: !Text
    , bidQty   :: !Text
    , askPrice :: !Text
    , askQty   :: !Text
    } deriving (Show, Eq, Generic)

instance FromJSON OrderBookTicker

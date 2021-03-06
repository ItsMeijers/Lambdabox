{-# LANGUAGE DeriveGeneric, DuplicateRecordFields #-}

module Binance.Internal.Trade.Types
    ( BinanceOrder(..)
    , ResponseType(..)
    , Side(..)
    , BinanceOrderResponse(..)
    , Fill(..)
    , OrderStatus(..)
    , CancelOrder(..)
    , PlacedOrder(..)
    ) where

import GHC.Generics
import Data.Aeson.Extended
import Data.Text (Text)

-- | Data for creating a order at Binance
data BinanceOrder = BinanceLimitOrder 
                        { tifLO        :: !Text
                        , qtyLO        :: !Double
                        , priceLO      :: !Double
                        , icebergQtyLO :: !(Maybe Double)
                        }
                  | BinanceMarketOrder { qtyMO :: !Double }
                  | BinanceStopLossOrder
                        { qtySLO       :: !Double
                        , stopPriceSLO :: !Double 
                        }
                  | BinanceStopLossLimitOrder
                        { qtySLLO        :: !Double
                        , stopPriceSLLO  :: !Double
                        , tifSLLO        :: !Text
                        , priceSLLO      :: !Double
                        , icebergQtySLLO :: !(Maybe Double)
                        }
                  | BinanceTakeProfit
                        { qtyTP       :: !Double
                        , stopPriceTP :: !Double
                        }
                  | BinanceTakeProfitLimit
                        { tifTPL        :: !Text
                        , qtyTPL        :: !Double
                        , priceTPL      :: !Double
                        , stopPriceTPL  :: !Double
                        , icebergQtyTPL :: !(Maybe Double)
                        }
                  | BinanceLimitMaker
                        { qtyLM   :: !Double
                        , priceLM :: !Double
                        }
                  deriving (Show, Eq)

data ResponseType = Acknowledge
                  | Result
                  | Full
                  deriving (Eq)

instance Show ResponseType where
    show Acknowledge = "ACK"
    show Result      = "RESULT"
    show Full        = "FULL"

data Side = Buy 
          | Sell
          deriving (Eq)
          
instance Show Side where
    show Buy  = "BUY"
    show Sell = "SELL"

data BinanceOrderResponse = OrderResponseAcknowledge
                               { symbol        :: !Text
                               , orderId       :: !Int
                               , clientOrderId :: !Text
                               , transactTime  :: !Int
                               } 
                          | OrderResponseResult
                               { symbol        :: !Text
                               , orderId       :: !Int
                               , clientOrderId :: !Text
                               , transactTime  :: !Int
                               , price         :: !Text
                               , origQty       :: !Text
                               , executedQty   :: !Text
                               , status        :: !Text
                               , timeInForce   :: !Text
                               , kind          :: !Text
                               , side          :: !Text
                               }                  
                          | OrderResponseFull
                               { symbol        :: !Text
                               , orderId       :: !Int
                               , clientOrderId :: !Text
                               , transactTime  :: !Int
                               , price         :: !Text
                               , origQty       :: !Text
                               , executedQty   :: !Text
                               , status        :: !Text
                               , timeInForce   :: !Text
                               , kind          :: !Text
                               , side          :: !Text
                               , fills         :: [Fill]
                               } 
                           deriving (Show, Eq, Generic)

instance FromJSON BinanceOrderResponse where
    parseJSON = genericParseJSON typeToKindOptions

data Fill = Fill
    { price           :: !Text
    , qty             :: !Text
    , commission      :: !Text
    , commissionAsset :: !Text
    } deriving (Show, Eq, Generic)

instance FromJSON Fill

data OrderStatus = OrderStatus
    { symbol        :: !Text
    , orderId       :: !Int
    , clientOrderId :: !Text
    , price         :: !Text
    , origQty       :: !Text
    , executedQty   :: !Text
    , status        :: !Text
    , timeInForce   :: !Text
    , kind          :: !Text
    , side          :: !Text
    , stopPrice     :: !Text
    , icebergQty    :: !Text
    , time          :: !Int
    , isWorking     :: !Bool
    } deriving (Show, Eq, Generic)
             
instance FromJSON OrderStatus where
    parseJSON = genericParseJSON typeToKindOptions

data CancelOrder = CancelOrder
    { symbol            :: !Text
    , origClientOrderId :: !Text
    , orderId           :: !Int
    , clientOrderId     :: !Text
    } deriving (Show, Eq, Generic)

instance FromJSON CancelOrder

data PlacedOrder = PlacedOrder
    { symbol        :: !Text
    , orderId       :: !Int
    , clientOrderId :: !Text
    , price         :: !Text
    , origQty       :: !Text
    , executedQty   :: !Text
    , status        :: !Text
    , timeInForce   :: !Text
    , kind          :: !Text
    , side          :: !Text
    , stopPrice     :: !Text
    , icebergQty    :: !Text
    , time          :: !Int
    , isWorking     :: !Bool
    } deriving (Show, Eq, Generic)

instance FromJSON PlacedOrder where
    parseJSON = genericParseJSON typeToKindOptions
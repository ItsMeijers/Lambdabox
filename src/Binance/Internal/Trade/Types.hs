{-# LANGUAGE DeriveGeneric, DuplicateRecordFields #-}

module Binance.Internal.Trade.Types
    ( BinanceOrder(..)
    , ResponseType(..)
    , Side(..)
    , TimeInForce(..)
    , OrderResponse(..)
    , Fill(..)
    , OrderStatus(..)
    , CancelOrder(..)
    , Order(..)
    ) where

import GHC.Generics
import Data.Aeson.Extended
import Data.Text (Text)

-- | Data for creating a order at Binance
data BinanceOrder = LimitOrder 
                        { tifLO        :: !TimeInForce
                        , qtyLO        :: !Double
                        , priceLO      :: !Double
                        , icebergQtyLO :: !(Maybe Double)
                        }
                  | MarketOrder { qtyMO :: !Double }
                  | StopLossOrder
                        { qtySLO       :: !Double
                        , stopPriceSLO :: !Double 
                        }
                  | StopLossLimitOrder
                        { qtySLLO        :: !Double
                        , stopPriceSLLO  :: !Double
                        , tifSLLO        :: !TimeInForce
                        , priceSLLO      :: !Double
                        , icebergQtySLLO :: !(Maybe Double)
                        }
                  | TakeProfit
                        { qtyTP       :: !Double
                        , stopPriceTP :: !Double
                        }
                  | TakeProfitLimit
                        { tifTPL        :: !TimeInForce
                        , qtyTPL        :: !Double
                        , priceTPL      :: !Double
                        , stopPriceTPL  :: !Double
                        , icebergQtyTPL :: !(Maybe Double)
                        }
                  | LimitMaker
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

data TimeInForce = GTC
                 | IOC
                 | FOK
                 deriving (Eq, Show)

data OrderResponse = OrderResponseAcknowledge
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

instance FromJSON OrderResponse where
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

data Order = Order
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

instance FromJSON Order where
    parseJSON = genericParseJSON typeToKindOptions
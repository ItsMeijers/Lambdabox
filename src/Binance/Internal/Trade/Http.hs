{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module Binance.Internal.Trade.Http
    ( trade
    , testTrade
    , currentOpenOrders
    , allOrders
    , queryOrderOnId
    , queryOrderOnClientId
    , cancelOrderOnId
    , cancelOrderOnClientId
    ) where

import Binance.Internal.Types
import Binance.Internal.Trade.Types
import Network.Wreq.Extended
import Data.Text (Text)
import Data.Aeson.Extended (Unit)

-- TODO TEST ALL THIS FUNCTIONS CAREFULLY!!!!!

-- | Create a trade on binance based on an BinanceOrder.
trade :: Text 
      -> Side 
      -> BinanceOrder
      -> Maybe Text 
      -> Maybe ResponseType 
      -> Maybe Int 
      -> Binance OrderResponse
trade symbol side binanceOrder newClientOrderId responseType recvWindow = 
    prepareTrade symbol side binanceOrder newClientOrderId responseType 
        recvWindow (postSigned "/api/v3/order")

-- | Test trade is like a trade but the actual binance order does not get
-- send to the matching engine. The Binance Unit tag is needed due to the 
-- FromJSON instance for empty bodies in Aeson this unit value gets replaced
-- with a typical haskel ()
testTrade :: Text 
          -> Side 
          -> BinanceOrder
          -> Maybe Text 
          -> Maybe ResponseType 
          -> Maybe Int 
          -> Binance ()
testTrade symbol side binanceOrder newClientOrderId responseType recvWindow = 
    prepareTrade symbol side binanceOrder newClientOrderId responseType 
        recvWindow  (fmap (const ()) . 
            (postSigned "/api/v3/order/test" :: [(Text, Text)] -> Binance Unit))

-- | Prepares the trade for sending for both the trade and testTrade function.            
prepareTrade :: Text 
             -> Side 
             -> BinanceOrder
             -> Maybe Text 
             -> Maybe ResponseType 
             -> Maybe Int
             -> ([(Text, Text)] -> Binance a) 
             -> Binance a
prepareTrade s side binanceOrder newClientOrderId responseType recvWindow f = 
    let standardParams = [("symbol", s), ("side", toText side)]
        orderParams    = paramsFromOrder binanceOrder
        maybeParams    = optionalParams [ "newClientOrderId" :? newClientOrderId
                                        , "newOrderRespType" :? responseType
                                        , "recvWindow"       :? recvWindow ]
        params         = standardParams ++ orderParams ++ maybeParams
    in f params

-- | Creates the parameter list based on the type of BinanceOrder and the 
-- record values that are within each type.
paramsFromOrder :: BinanceOrder -> [(Text, Text)]
paramsFromOrder LimitOrder { tifLO, qtyLO, priceLO, icebergQtyLO } =
    [ ("type", "LIMIT")
    , ("timeInForce", toText tifLO)
    , ("quantity", toText qtyLO)
    , ("price", toText priceLO)
    ] ++ optionalParams ["icebergQty" :? icebergQtyLO]
paramsFromOrder MarketOrder { qtyMO } =
    [ ("type", "MARKET")
    , ("quantity", toText qtyMO)
    ]
paramsFromOrder StopLossOrder { qtySLO, stopPriceSLO} =
    [ ("type", "STOP_LOSS")
    , ("quantity", toText qtySLO)
    , ("stopPrice", toText stopPriceSLO)
    ]
paramsFromOrder StopLossLimitOrder 
        { qtySLLO, stopPriceSLLO, tifSLLO, priceSLLO, icebergQtySLLO } =
    [ ("type", "STOP_LOSS_LIMIT")
    , ("quantity", toText qtySLLO)
    , ("stopPrice", toText stopPriceSLLO)
    , ("timeInForce", toText tifSLLO)
    , ("price", toText priceSLLO)
    ] ++ optionalParams ["icebergQty" :? icebergQtySLLO]
paramsFromOrder TakeProfit { qtyTP, stopPriceTP } =
    [ ("type", "TAKE_PROFIT")
    , ("quantity", toText qtyTP)
    , ("stopPrice", toText stopPriceTP)
    ]
paramsFromOrder TakeProfitLimit 
        { tifTPL, qtyTPL, priceTPL, stopPriceTPL, icebergQtyTPL } =
    [ ("type", "TAKE_PROFIT_LIMIT")
    , ("timeInForce", toText tifTPL)
    , ("quantity", toText qtyTPL)
    , ("price", toText priceTPL)
    , ("stopPrice", toText stopPriceTPL)
    ] ++ optionalParams ["icebergQty" :? icebergQtyTPL]
paramsFromOrder LimitMaker { qtyLM, priceLM } =
    [ ("type", "LIMIT_MAKER")
    , ("quantity", toText qtyLM)
    , ("price", toText priceLM)
    ]

queryOrderOnId :: Text -> Int -> Maybe Int -> Binance Order
queryOrderOnId symbol orderId recvWindow = 
    getSigned "/api/v3/order" $ [ ("symbol", symbol)
                                , ("orderId", toText orderId)
                                ] ++ optionalParams ["recvWindow" :? recvWindow] 

queryOrderOnClientId :: Text -> Text -> Maybe Int -> Binance Order
queryOrderOnClientId symbol origClientOrderId recvWindow = 
    getSigned "/api/v3/order" $ [ ("symbol", symbol)
                                , ("origClientOrderId", origClientOrderId)
                                ] ++ optionalParams ["recvWindow" :? recvWindow]

cancelOrderOnId :: Text 
                -> Int  
                -> Maybe Text 
                -> Maybe Int 
                -> Binance CancelOrder
cancelOrderOnId symbol orderId newClientOrderId recvWindow = 
    deleteSigned "/api/v3/order" $ [ ("symbol", symbol)
                                   , ("orderId", toText orderId)
                                   ] ++ optionalParams
                                   [ "newClientOrderId" :? newClientOrderId
                                   , "recvWindow" :? recvWindow
                                   ]

cancelOrderOnClientId :: Text 
                -> Text  
                -> Maybe Text 
                -> Maybe Int 
                -> Binance CancelOrder
cancelOrderOnClientId symbol origClientOrderId newClientOrderId recvWindow = 
    deleteSigned "/api/v3/order" $ [ ("symbol", symbol)
                                    , ("orderId", origClientOrderId)
                                    ] ++ optionalParams
                                    [ "newClientOrderId" :? newClientOrderId
                                    , "recvWindow" :? recvWindow
                                    ]

-- | List all the order status that are currently open on Binance.
currentOpenOrders :: Maybe Text -> Maybe Int -> Binance [OrderStatus]
currentOpenOrders symbol recvWindow = 
    getSigned "/api/v3/openOrders" $ optionalParams 
                            ["symbol" :? symbol, "recvWindow" :? recvWindow]

-- | List all the orders on Binance.
allOrders :: Text -> Maybe Int -> Maybe Int -> Maybe Int -> Binance [Order]
allOrders symbol orderId limit recvWindow =
    getSigned "/api/v3/allOrders" $ [("symbol", symbol)] ++ optionalParams
        ["orderId" :? orderId, "limit" :? limit, "recvWindow" :? recvWindow]
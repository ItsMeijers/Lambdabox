{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}

module Lambdabox.Trade.TimeInForce
    ( TimeInForce(..)
    ) where

import Lambdabox.Translate
import Lambdabox.Exchange
import Data.Text (Text)

data TimeInForce = GoodTilCanceled
                 | ImmediateOrCancel
                 | FillOrKill
                 deriving (Show, Eq)

instance Translate Binance TimeInForce Text where
    translate Binance GoodTilCanceled   = "GTC"
    translate Binance ImmediateOrCancel = "IOC"
    translate Binance FillOrKill        = "FOK"
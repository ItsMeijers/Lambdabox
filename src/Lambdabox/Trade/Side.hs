{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}

module Lambdabox.Trade.Side
    ( Side(..)
    , ExchangePairSide
    , buy
    , sell
    ) where

import Lambdabox.ExchangePair
import Lambdabox.Translate
import Lambdabox.Exchange

import Data.Text (Text)

data Side = Buy
          | Sell
          deriving (Show, Eq)

instance Translate Binance Side Text where
    translate _ Buy  = "BUY"
    translate _ Sell = "SELL"

type ExchangePairSide e a b = (ExchangePair e a b, Side) 

buy :: ExchangePair e a b -> ExchangePairSide e a b
buy ep = (ep, Buy)

sell :: ExchangePair e a b -> ExchangePairSide e a b
sell ep = (ep, Sell)
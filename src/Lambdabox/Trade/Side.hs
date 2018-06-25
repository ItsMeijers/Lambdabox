module Lambdabox.Trade.Side
    ( Side
    , ExchangePairSide
    , buy
    , sell
    ) where

import Lambdabox.ExchangePair

data Side = Buy
          | Sell
          deriving (Show, Eq)

data ExchangePairSide e a b = ExchangePairSide e a b Side 
                            deriving (Eq, Show)

buy :: ExchangePair e a b -> ExchangePairSide e a b
buy (ExchangePair e a b) = ExchangePairSide e a b Buy

sell :: ExchangePair e a b -> ExchangePairSide e a b
sell (ExchangePair e a b) = ExchangePairSide e a b Sell
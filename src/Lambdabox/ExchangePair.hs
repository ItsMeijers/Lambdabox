module Lambdabox.ExchangePair
    ( ExchangePair(..) -- Not exporting the constructor to force using combinators
    , (//)
    , on
    ) where

import Lambdabox.Exchange
import Lambdabox.Symbol

data TradingPair a b = TradingPair a b
                    deriving (Show, Eq)

data ExchangePair e a b = ExchangePair e a b 
                          deriving (Eq, Show)

-- | Combinator constructor for creating a symbol where a and b both need to
-- have a Currency instance.
(//) :: (Symbol a, Symbol b) => a -> b -> TradingPair a b
(//) a b = TradingPair a b

-- | Combinator constructor for creating a Exchange Symbol based on an e that
-- has an Exchange instance and a Symbol that has a Currency instance for both
-- a and b
on :: (Symbol a, Symbol b, Exchange e) => TradingPair a b -> e -> ExchangePair e a b
on (TradingPair a b) e = ExchangePair e a b
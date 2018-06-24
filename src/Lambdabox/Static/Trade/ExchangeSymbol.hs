module Lambdabox.Static.Trade.ExchangeSymbol
    ( ExchangeSymbol -- Not exporting the constructor to force using combinators
    , for
    , on
    ) where

import Lambdabox.Static.Exchange
import Lambdabox.Static.Currency

data Symbol a b = Symbol a b
                deriving (Show, Eq)

data ExchangeSymbol e a b = ExchangeSymbol e a b 
                          deriving (Eq, Show)

-- | Combinator constructor for creating a symbol where a and b both need to
-- have a Currency instance.
for :: (Currency a, Currency b) => a -> b -> Symbol a b
for a b = Symbol a b

-- | Combinator constructor for creating a Exchange Symbol based on an e that
-- has an Exchange instance and a Symbol that has a Currency instance for both
-- a and b
on :: (Currency a, Currency b, Exchange e) => Symbol a b -> e -> ExchangeSymbol e a b
on (Symbol a b) e = ExchangeSymbol e a b
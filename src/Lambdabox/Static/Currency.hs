module Lambdabox.Static.Currency
    ( Currency
    , BTC(..)
    , ADA(..)
    ) where

class Currency a where

data BTC = BTC deriving Show
data ADA = ADA deriving Show

instance Currency BTC
instance Currency ADA
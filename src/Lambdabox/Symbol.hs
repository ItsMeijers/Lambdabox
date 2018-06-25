module Lambdabox.Symbol
    ( Symbol
    , BTC(..)
    , ADA(..)
    ) where

class Symbol a where

data BTC = BTC deriving Show
data ADA = ADA deriving Show

instance Symbol BTC
instance Symbol ADA
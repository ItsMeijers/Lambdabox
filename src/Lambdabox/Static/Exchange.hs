module Lambdabox.Static.Exchange
    ( Exchange
    , Binance(..)
    ) where

class Exchange e where

data Binance = Binance deriving Show

instance Exchange Binance
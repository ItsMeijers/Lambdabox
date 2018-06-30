module Lambdabox.Exchange
    ( Exchange(..)
    , Binance(..)
    ) where

-- | See if there is a smarter way than doing the isBinance boolean check
-- it is safe in the sense of compile time checks, but not very pretty
class Exchange e where
    isBinance :: e -> Bool

data Binance = Binance deriving Show

instance Exchange Binance where
    isBinance Binance = True
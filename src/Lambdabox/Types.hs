module Lambdabox.Types
    ( Order(..)
    , OrderResponse(..)
    , TimeInForce(..)
    ) where



data Order = Order { testOrder :: String }

data OrderResponse = OrderResponse { test :: Int }

data TimeInForce = GoodTilCanceled
                 | ImmediateOrCancel
                 | FillOrKill
                 deriving (Show, Eq)
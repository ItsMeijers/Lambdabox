module Binance.Internal.Http
    ( runBinance
    ) where

import Binance.Internal.Types
import Control.Monad.Reader
import Control.Monad.Except

-- | Run the Binance Transformer stack
runBinance ::  Binance a -> BinanceOptions -> IO (Either Error a)
runBinance ba = runExceptT . runReaderT (run ba)
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}

module Binance.Internal.Types 
    ( BinanceOptions(..)
    , Binance(..)
    , Error(..)
    ) where

import GHC.Generics
import Data.Aeson.Extended
import Data.Text (Text)
import Control.Monad.Reader
import Control.Monad.Except

data BinanceOptions = BinanceOptions
    { apiKey    :: !Text
    , secretKey :: !Text
    , orderBookRetries :: !Int
    } deriving (Show, Eq)

-- Add State monad for tracking the weight for each request!
newtype Binance a = Binance
    { run :: ReaderT BinanceOptions (ExceptT Error IO) a
    } deriving ( Monad
               , Functor
               , Applicative
               , MonadReader BinanceOptions
               , MonadIO
               , MonadError Error)

data Error = Error
    { code :: !Int
    , msg  :: !Text
    } deriving (Show, Eq, Generic)

instance FromJSON Error
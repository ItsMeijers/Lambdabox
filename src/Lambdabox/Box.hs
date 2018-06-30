{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}

module Lambdabox.Box 
    ( BoxConfiguration(..)
    , Box(..)
    , Error(..)
    , runBox
    ) where

import GHC.Generics
import Data.Aeson.Extended
import Data.Text (Text)
import Control.Monad.Reader
import Control.Monad.Except

data BoxConfiguration = BoxConfiguration
    { apiKey    :: !Text
    , secretKey :: !Text
    } deriving (Show, Eq)

newtype Box a = Box
    { run :: ReaderT BoxConfiguration (ExceptT Error IO) a
    } deriving ( Monad
               , Functor
               , Applicative
               , MonadReader BoxConfiguration
               , MonadIO
               , MonadError Error)

-- | Run the Binance Transformer stack
runBox ::  Box a -> BoxConfiguration -> IO (Either Error a)
runBox bc = runExceptT . runReaderT (run bc)

data Error = Error
    { code :: !Int
    , msg  :: !Text
    } deriving (Show, Eq, Generic)

instance FromJSON Error
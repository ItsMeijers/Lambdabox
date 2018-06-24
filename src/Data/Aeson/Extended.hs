{-# LANGUAGE OverloadedStrings #-}

module Data.Aeson.Extended
    ( module Data.Aeson
    , module Data.Aeson.Types
    , typeToKindOptions
    , Unit(..)
    ) where

import Data.Aeson
import Data.Aeson.Types        

typeToKindOptions :: Options
typeToKindOptions  = defaultOptions { fieldLabelModifier = t2k }
    where t2k "kind" = "type"
          t2k name   = name

data Unit = Unit

instance FromJSON Unit where 
    parseJSON _ = pure Unit
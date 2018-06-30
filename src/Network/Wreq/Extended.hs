{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances, FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Network.Wreq.Extended
    ( module Network.Wreq
    , get
    , post
    , put
    , delete
    , getSigned
    , postSigned
    , deleteSigned
    , optionalParams
    , ToText(..)
    , OptionalParameter(..)
    ) where

import              Lambdabox.Box
import              Network.Wreq hiding (get, post, delete, put)
import              Control.Lens ((&), (^.), (^?), (.~), set)
import              Control.Monad.Reader
import              Control.Monad.Except
import              Data.Aeson (FromJSON)
import              Data.Aeson.Lens (key)
import              Data.Maybe (catMaybes)
import              Data.Text (Text, pack, append)
import              Data.Text.Encoding (encodeUtf8, decodeUtf8)
import              Data.ByteString.Lazy (ByteString, empty)
import              Data.Time.Clock.POSIX (getPOSIXTime)
import              Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import              Crypto.Hash (Digest)
import              Crypto.Hash.Algorithms (SHA256)
import qualified    Data.Text as T
import qualified    Network.HTTP.Client as N
import qualified    Control.Exception   as E

get :: forall a. (FromJSON a)
    => String 
    -> [(Text, Text)]
    -> Box a
get uri params = httpRequest getWith uri params

post :: forall a. (FromJSON a)
     => String 
     -> [(Text, Text)]
     -> Box a
post uri params = httpRequest (\o s -> postWith o s empty) uri params

put :: forall a. (FromJSON a)
    => String 
    -> [(Text, Text)]
    -> Box a
put uri params = httpRequest (\o s -> putWith o s empty) uri params

delete :: forall a. (FromJSON a)
       => String 
       -> [(Text, Text)]
       -> Box a
delete uri params = httpRequest deleteWith uri params

getSigned :: forall a. (FromJSON a) 
           => String
           -> [(Text, Text)]
           -> Box a
getSigned uri params = signed params (get uri)

postSigned :: forall a. (FromJSON a) => String -> [(Text, Text)] -> Box a
postSigned uri params = signed params (post uri)

deleteSigned :: forall a. (FromJSON a) => String -> [(Text, Text)] -> Box a
deleteSigned uri params = signed params (delete uri)

signed :: forall a. (FromJSON a) 
           => [(Text, Text)]
           -> ([(Text, Text)] -> Box a) -- Continuation Request
           -> Box a
signed params cRequest = do
    boxConfiguration <- ask
    timestamp      <- liftIO getTimeStamp
    let paramsT    =  ("timestamp", timestamp) : params 
        signature  =  sign paramsT (secretKey boxConfiguration)
    cRequest (("signature", signature) : paramsT)  

-- | Return timestamp in milliseconds
getTimeStamp :: IO Text
getTimeStamp = (pack . show . round . (* 1000)) <$> getPOSIXTime

-- | Sign the request by hashing the url parameters and secret key using SHA256
sign :: [(Text, Text)] -> Text -> Text
sign params secretKey = toText $ hmac' secretKey message
    where message           = T.drop 1 $ foldr keyValue "" params
          keyValue (k, v) m = m `append` "&" `append` k `append` "=" `append` v

hmac' :: Text -> Text -> Digest SHA256 
hmac' sk = hmacGetDigest . hmac (encodeUtf8 sk) . encodeUtf8

httpRequest :: forall a. (FromJSON a)
            => (Options -> String -> IO (Response ByteString))
            -> String 
            -> [(Text, Text)]
            -> Box a
httpRequest request uri params = do
    boxConfiguration <- ask
    response       <- liftIO $ request 
                                (buildOptions boxConfiguration params) 
                                ("https://api.binance.com" ++ uri)
    liftEither $ foldResponse (response ^. responseStatus . statusCode) response 

-- | Build the options for the http request based on a list of key value 
-- parameters and setting the option of not checking the response and throwing
-- an IO error.
buildOptions :: BoxConfiguration -> [(Text, Text)] -> Options
buildOptions bo = foldl (\o (k, v) -> o & param k .~ [v]) (defaults' & key) 
    where defaults' = set checkResponse (Just $ \_ _ -> return ()) defaults
          key       = header "X-MBX-APIKEY" .~ [encodeUtf8 $ apiKey bo]

    -- | Fold the response in Either an Error or the specific data
foldResponse :: (FromJSON a) => Int -> Response ByteString -> Either Error a
foldResponse s r
    | s <= 207 && s >= 200 = liftJSONException (asJSON r)
    | otherwise            = do
        error <- liftJSONException (asJSON r)
        Left error
    
-- | If JSOn extraction goes wrong the error gets transformed into the Error
-- Data type
liftJSONException :: Either E.SomeException (Response a) -> Either Error a
liftJSONException (Right x) = Right (N.responseBody x)                 
liftJSONException (Left se) = Left $ Error 
                                        { code = -1
                                        , msg = pack $ E.displayException se}

class ToText a where
    toText :: a -> Text

instance {-# OVERLAPPING #-} ToText Text where
    toText = id
    
instance Show a => ToText a where
    toText = pack . show

data OptionalParameter = forall s. ToText s => (:?) Text (Maybe s)

-- | Concatenates a list of key and a maybe value for easy
optionalParams :: [OptionalParameter] -> [(Text, Text)]
optionalParams = catMaybes . fmap fromMaybeParam
        where fromMaybeParam (k :? (Just v)) = Just (k, toText v)
              fromMaybeParam _               = Nothing
module Binance.Error where

-- | TODO is this data type required due to most of the errors are caught at
-- compile time?
data BinanceError = BinanceError
    { kind    :: BinanceErrorKind
    , message :: String 
    } deriving (Show, Eq)

data BinanceErrorKind = Unknown
                      | Disconnected
                      | Unauthorized
                      | TooManyRequests
                      | UnexpectedResponse
                      | TimeOut
                      | ErrorMessageReceived
                      | InvalidMessage
                      | UnkonwnOrderComposition
                      | TooManyOrders
                      | ServiceShuttingDown
                      | UnsupportedOperation
                      | InvalidTimestamp
                      | InvalidSignature
                      | IllegalChars
                      | TooManyParameters
                      | MandatoryParameterEmptyOrMalformed
                      | UnknownParameter
                      | UnreadParameters
                      | ParamEmpty
                      | ParamNotRequired
                      | NoDepth
                      | TimeInForceNotRequired
                      | InvalidTimeInForce
                      | InvalidOrderType
                      | InvalidSide
                      | EmptyNewClientOrderId
                      | EmptyOriginalClientOrderId
                      | BadInterval
                      | BadSymbol
                      | InvalidListenKey
                      | MoreThanXXHours
                      | OptionalParametersBadCombo
                      | InvalidParameter
                      | BadApiId
                      | DuplicateApiKeyDescription
                      | NewOrderRejected
                      | CancelRejected
                      | CancelAllFail
                      | NoSuchOrder
                      | BadApiKeyFormat
                      | RejectedMbxKey
                      deriving (Show, Eq)

fromCode :: Int -> BinanceErrorKind
fromCode -1000 = Unknown
fromCode -1001 = Disconnected
fromCode -1002 = Unauthorized
fromCode -1003 = TooManyRequests
fromCode -1006 = UnexpectedResponse
fromCode -1007 = TimeOut
fromCode -1010 = ErrorMessageReceived
fromCode -1013 = InvalidMessage
fromCode -1014 = UnkonwnOrderComposition
fromCode -1015 = TooManyOrders
fromCode -1016 = ServiceShuttingDown
fromCode -1020 = UnsupportedOperation
fromCode -1021 = InvalidTimestamp
fromCode -1022 = InvalidSignature
fromCode -1100 = IllegalChars
fromCode -1101 = TooManyParameters
fromCode -1102 = MandatoryParameterEmptyOrMalformed
fromCode -1103 = UnknownParameter
fromCode -1104 = UnreadParameters
fromCode -1105 = ParamEmpty
fromCode -1106 = ParamNotRequired
fromCode -1112 = NoDepth
fromCode -1114 = TimeInForceNotRequired
fromCode -1115 = InvalidTimeInForce
fromCode -1116 = InvalidOrderType
fromCode -1117 = InvalidSide
fromCode -1118 = EmptyNewClientOrderId
fromCode -1119 = EmptyOriginalClientOrderId
fromCode -1120 = BadInterval
fromCode -1121 = BadSymbol
fromCode -1125 = InvalidListenKey
fromCode -1127 = MoreThanXXHours
fromCode -1128 = OptionalParametersBadCombo
fromCode -1130 = InvalidParameter
fromCode -2008 = BadApiId
fromCode -2009 = DuplicateApiKeyDescription
fromCode -2010 = NewOrderRejected
fromCode -2011 = CancelRejected
fromCode -2012 = CancelAllFail
fromCode -2013 = NoSuchOrder
fromCode -2014 = BadApiKeyFormat
fromCode -2015 = RejectedMbxKey
fromCode _     = Unknown

toCode :: BinanceErrorKind -> Int
toCode Unknown                            = -1000
toCode Disconnected                       = -1001
toCode Unauthorized                       = -1002
toCode TooManyRequests                    = -1003
toCode UnexpectedResponse                 = -1006
toCode TimeOut                            = -1007
toCode ErrorMessageReceived               = -1010
toCode InvalidMessage                     = -1013
toCode UnkonwnOrderComposition            = -1014
toCode TooManyOrders                      = -1015
toCode ServiceShuttingDown                = -1016
toCode UnsupportedOperation               = -1020
toCode InvalidTimestamp                   = -1021
toCode InvalidSignature                   = -1022
toCode IllegalChars                       = -1100
toCode TooManyParameters                  = -1101
toCode MandatoryParameterEmptyOrMalformed = -1102
toCode UnknownParameter                   = -1103
toCode UnreadParameters                   = -1104
toCode ParamEmpty                         = -1105
toCode ParamNotRequired                   = -1106
toCode NoDepth                            = -1112
toCode TimeInForceNotRequired             = -1114
toCode InvalidTimeInForce                 = -1115
toCode InvalidOrderType                   = -1116
toCode InvalidSide                        = -1117
toCode EmptyNewClientOrderId              = -1118
toCode EmptyOriginalClientOrderId         = -1119
toCode BadInterval                        = -1120
toCode BadSymbol                          = -1121
toCode InvalidListenKey                   = -1125
toCode MoreThanXXHours                    = -1127
toCode OptionalParametersBadCombo         = -1128
toCode InvalidParameter                   = -1130
toCode BadApiId                           = -2008
toCode DuplicateApiKeyDescription         = -2009
toCode NewOrderRejected                   = -2010
toCode CancelRejected                     = -2011
toCode CancelAllFail                      = -2012
toCode NoSuchOrder                        = -2013
toCode BadApiKeyFormat                    = -2014
toCode RejectedMbxKey                     = -2015    
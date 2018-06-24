module Lambdabox.Box
    ( Box(..)
    , runBox
    ) where

type Box a = Configuration -> IO a

data Configuration = Configuration 
    { testing :: Bool
    } deriving (Show, Eq)

runBox :: Configuration -> Box a -> IO a
runBox = undefined
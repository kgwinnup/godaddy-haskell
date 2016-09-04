{-# LANGUAGE OverloadedStrings #-}

module Network.GoDaddy.ErrorTypes ( GError (GError)
                                  , Fields (Fields)
                                  ) where

import           Data.Aeson

data GError = GError { erCode          :: Maybe String
                     , erMessage       :: Maybe String
                     , erRetryAfterSec :: Maybe Integer
                     , erName          :: Maybe String
                     , erFields        :: Maybe [Fields] } deriving (Show)

instance ToJSON GError where
  toJSON (GError c m r n f) =
    object [ "code" .= c
           , "message" .= m
           , "retryAfterSec" .= r
           , "name" .= n
           , "fields" .= f ]

instance FromJSON GError where
  parseJSON (Object v) =
    GError <$> v .:? "code"
           <*> v .:? "message"
           <*> v .:? "retryAfterSec"
           <*> v .:? "name"
           <*> v .:? "fields"
  parseJSON _ = fail "Error object not found"

data Fields = Fields { fiPath        :: String
                     , fiPathRelated :: Maybe String
                     , fiCode        :: String
                     , fiMessage     :: Maybe String } deriving (Show)

instance ToJSON Fields where
  toJSON (Fields p pr c m) =
    object [ "path" .= p
           , "pathRelated" .= pr
           , "code" .= c
           , "message" .= m ]

instance FromJSON Fields where
  parseJSON (Object v) =
    Fields <$> v .: "path"
           <*> v .:? "pathRelated"
           <*> v .: "code"
           <*> v .:? "message"
  parseJSON _ = fail "Fields object not found"





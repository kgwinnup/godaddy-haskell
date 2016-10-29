{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Network.GoDaddy.ErrorTypes ( GError (GError)
                                  , Fields (Fields)
                                  ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Char
import           GHC.Generics

data GError = GError { eCode          :: Maybe String
                     , eMessage       :: Maybe String
                     , eRetryAfterSec :: Maybe Integer
                     , eName          :: Maybe String
                     , eFields        :: Maybe [Fields] } deriving (Generic, Show)

instance ToJSON GError where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = (\x -> map toLower $ drop 1 x) }

instance FromJSON GError where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = (\x -> map toLower $ drop 1 x) }

data Fields = Fields { fPath        :: String
                     , fPathRelated :: Maybe String
                     , fCode        :: String
                     , fMessage     :: Maybe String } deriving (Generic, Show)

instance ToJSON Fields where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = (\x -> map toLower $ drop 1 x) }

instance FromJSON Fields where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = (\x -> map toLower $ drop 1 x) }



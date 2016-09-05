{-# LANGUAGE OverloadedStrings #-}

module Network.GoDaddy.AgreementTypes (LegalAgreement(LegalAgreement)) where

import           Data.Aeson

data LegalAgreement = LegalAgreement { agreementKey :: String
                                     , title        :: String
                                     , url          :: Maybe String
                                     , content      :: String } deriving (Show)

instance ToJSON LegalAgreement where
  toJSON (LegalAgreement a t u c) =
    object [ "agreementKey" .= a
           , "title" .= t
           , "url" .= u
           , "content" .= c ]

instance FromJSON LegalAgreement where
  parseJSON (Object v) =
    LegalAgreement <$> v .: "agreementKey"
                   <*> v .: "title"
                   <*> v .:? "url"
                   <*> v .: "content"
  parseJSON _ = fail "LegalAgreement object not found"
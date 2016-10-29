{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Network.GoDaddy.AgreementTypes (LegalAgreement(LegalAgreement)) where

import           Data.Aeson
import           Data.Aeson.TH

data LegalAgreement = LegalAgreement { agreementKey :: String
                                     , title        :: String
                                     , url          :: Maybe String
                                     , content      :: String } deriving (Show)
$(deriveJSON defaultOptions ''LegalAgreement)

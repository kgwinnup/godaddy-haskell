{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Network.GoDaddy.AftermarketTypes ( AftermarketListingExpiryCreate(AftermarketListingExpiryCreate)
                                        , AftermarketListingAction(AftermarketListingAction)) where

import           Data.Aeson
import           Data.Aeson.TH

data AftermarketListingExpiryCreate = AftermarketListingExpiryCreate { domain            :: String
                                                                     , expiresAt         :: String
                                                                     , losingRegistrarId :: Integer
                                                                     , pageViewsMonthly  :: Maybe Integer
                                                                     , revenueMonthly    :: Maybe Integer } deriving (Show)
$(deriveJSON defaultOptions ''AftermarketListingExpiryCreate)

data AftermarketListingAction = AftermarketListingAction { listingActionId :: Integer } deriving (Show)
$(deriveJSON defaultOptions ''AftermarketListingAction)



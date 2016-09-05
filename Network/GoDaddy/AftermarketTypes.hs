{-# LANGUAGE OverloadedStrings #-}

module Network.GoDaddy.AftermarketTypes ( AftermarketListingExpiryCreate(AftermarketListingExpiryCreate)
                                        , AftermarketListingAction(AftermarketListingAction)) where

import           Data.Aeson

data AftermarketListingExpiryCreate = AftermarketListingExpiryCreate { domain            :: String
                                                                     , expiresAt         :: String
                                                                     , losingRegistrarId :: Integer
                                                                     , pageViewsMonthly  :: Maybe Integer
                                                                     , revenueMonthly    :: Maybe Integer } deriving (Show)

instance ToJSON AftermarketListingExpiryCreate where
  toJSON (AftermarketListingExpiryCreate d e l p r) =
    object [ "domain" .= d
           , "expiresAt" .= e
           , "losingRegistrarId" .= l
           , "pageViewsMonthly" .= p
           , "revenueMonthly" .= r ]

instance FromJSON AftermarketListingExpiryCreate where
  parseJSON (Object v) =
    AftermarketListingExpiryCreate <$> v .: "domain"
                                   <*> v .: "expiresAt"
                                   <*> v .: "losingRegistrarId"
                                   <*> v .:? "pageViewsMonthly"
                                   <*> v .:? "revenueMonthly"
  parseJSON _ = fail "AftermarketListingExpiryCreate object not found"

data AftermarketListingAction = AftermarketListingAction { listingActionId :: Integer } deriving (Show)

instance ToJSON AftermarketListingAction where
  toJSON (AftermarketListingAction id) =
    object [ "listingActionId" .= id ]

instance FromJSON AftermarketListingAction where
  parseJSON (Object v) =
    AftermarketListingAction <$> v .: "listingActionId"
  parseJSON _ = fail "AftermarketListingAction object not found"





{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Network.GoDaddy.AbuseTypes ( Pagination(Pagination)
                                  , AbuseTicketList(AbuseTicketList)
                                  , AbuseTicketCreate(AbuseTicketCreate)
                                  , AbuseTicketId(AbuseTicketId)
                                  , AbuseTicket(AbuseTicket)) where


import           Data.Aeson
import           Data.Aeson.TH
import           Data.Char
import           GHC.Generics

data AbuseTicketId = AbuseTicketId { ticketId :: String } deriving (Show)
$(deriveJSON defaultOptions ''AbuseTicketId)

data Pagination = Pagination { first    :: Maybe String
                             , previous :: Maybe String
                             , next     :: Maybe String
                             , last     :: Maybe String
                             , total    :: Maybe Integer } deriving (Show)
$(deriveJSON defaultOptions ''Pagination)

data AbuseTicketList = AbuseTicketList { ticketIds  :: [AbuseTicketId]
                                       , pagination :: Maybe Pagination } deriving (Show)
$(deriveJSON defaultOptions ''AbuseTicketList)

data AbuseTicketCreate = AbuseTicketCreate { abuseType        :: String
                                           , abuseSource      :: String
                                           , abuseTarget      :: Maybe String
                                           , abuseProxy       :: Maybe String
                                           , abuseIntentional :: Maybe Bool
                                           , abuseInfo        :: Maybe String
                                           , abuseInfoUrl     :: Maybe String } deriving (Generic, Show)

instance ToJSON AbuseTicketCreate where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = (\x -> (toLower $ head x):[] ++ (tail x)) . drop 5 }

instance FromJSON AbuseTicketCreate where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = (\x -> (toLower $ head x):[] ++ (tail x)) . drop 5 }

data AbuseTicket = AbuseTicket { id        :: String
                               , reporter  :: String
                               , domainIp  :: String
                               , close     :: Bool
                               , typ       :: String
                               , target    :: String
                               , source    :: String
                               , createdAt :: String
                               , closedAt  :: String } deriving (Show)

$(deriveJSON defaultOptions ''AbuseTicket)



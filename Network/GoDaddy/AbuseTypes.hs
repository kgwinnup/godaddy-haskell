{-# LANGUAGE OverloadedStrings #-}

module Network.GoDaddy.AbuseTypes ( Pagination(Pagination)
                                  , AbuseTicketList(AbuseTicketList)
                                  , AbuseTicketCreate(AbuseTicketCreate)
                                  , AbuseTicketId(AbuseTicketId)
                                  , AbuseTicket(AbuseTicket)) where


import           Data.Aeson

data Pagination = Pagination { first    :: Maybe String
                             , previous :: Maybe String
                             , next     :: Maybe String
                             , last     :: Maybe String
                             , total    :: Maybe Integer } deriving (Show)

instance ToJSON Pagination where
  toJSON (Pagination f p n l t) =
    object [ "first" .= f
           , "previous" .= p
           , "next" .= n
           , "last" .= l
           , "total" .= t ]

instance FromJSON Pagination where
  parseJSON (Object v) =
    Pagination <$> v .:? "first"
               <*> v .:? "previous"
               <*> v .:? "next"
               <*> v .:? "last"
               <*> v .:? "total"
  parseJSON _ = fail "Pagination object not found"

data AbuseTicketList = AbuseTicketList { ticketIds  :: [AbuseTicketId]
                                       , pagination :: Maybe Pagination } deriving (Show)

instance ToJSON AbuseTicketList where
  toJSON (AbuseTicketList t p) =
    object [ "ticketIds" .= t
           , "pagination" .= p ]

instance FromJSON AbuseTicketList where
  parseJSON (Object v) =
    AbuseTicketList <$> v .: "ticketIds"
                    <*> v .:? "pagination"
  parseJSON _ = fail "AbuseTicketList object not found"

data AbuseTicketCreate = AbuseTicketCreate { abuseType :: String
                                           , abuseSource :: String
                                           , abuseTarget :: Maybe String
                                           , abuseProxy :: Maybe String
                                           , abuseIntentional :: Maybe Bool
                                           , abuseInfo :: Maybe String
                                           , abuseInfoUrl :: Maybe String } deriving (Show)

instance ToJSON AbuseTicketCreate where
  toJSON (AbuseTicketCreate ty so ta pr int inf infu) =
    object [ "type" .= ty
           , "source" .= so
           , "target" .= ta
           , "proxy" .= pr
           , "intentional" .= int
           , "info" .= inf
           , "infoUrl" .= infu ]

instance FromJSON AbuseTicketCreate where
  parseJSON (Object v) =
    AbuseTicketCreate <$> v .: "type"
                      <*> v .: "source"
                      <*> v .:? "target"
                      <*> v .:? "proxy"
                      <*> v .:? "intentional"
                      <*> v .:? "info"
                      <*> v .:? "infoUrl"
  parseJSON _ = fail "AbuseTicketCreate object not found"

data AbuseTicketId = AbuseTicketId { ticketId :: String } deriving (Show)

instance ToJSON AbuseTicketId where
  toJSON (AbuseTicketId t) = object [ "ticketId" .= t ]

instance FromJSON AbuseTicketId where
  parseJSON (Object v) = AbuseTicketId <$> v .: "ticketId"
  parseJSON _ = fail "AbuseTicketId object not found"

data AbuseTicket = AbuseTicket { id        :: String
                               , reporter  :: String
                               , domainIp  :: String
                               , close     :: Bool
                               , typ       :: String
                               , target    :: String
                               , source    :: String
                               , createdAt :: String
                               , closedAt  :: String } deriving (Show)

instance ToJSON AbuseTicket where
  toJSON (AbuseTicket i r d c ty ta s cr cl) =
    object [ "ticketId" .= i
           , "reporter" .= r
           , "domainIp" .= d
           , "closed" .= c
           , "type" .= ty
           , "target" .= ta
           , "source" .= s
           , "createdAt" .= cr
           , "closedAt" .= cl ]

instance FromJSON AbuseTicket where
  parseJSON (Object v) =
    AbuseTicket <$> v .: "ticketId"
                <*> v .: "reporter"
                <*> v .: "domainIp"
                <*> v .: "closed"
                <*> v .: "type"
                <*> v .: "target"
                <*> v .: "source"
                <*> v .: "createdAt"
                <*> v .: "closedAt"



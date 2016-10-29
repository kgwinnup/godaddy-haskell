{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Network.GoDaddy.DomainTypes ( Contact(Contact)
                                   , Contacts(Contacts)
                                   , AddressMailing(AddressMailing)
                                   , DomainSummary(DomainSummary)
                                   , DomainUpdate(DomainUpdate)
                                   , PrivacyPurchase(PrivacyPurchase)
                                   , DomainPurchaseResponse(DomainPurchaseResponse)
                                   , DNSRecord(DNSRecord)
                                   , DomainTransferIn(DomainTransferIn)
                                   , LegalAgreement(LegalAgreement)
                                   , DomainAvailableResponse(DomainAvailableResponse)
                                   , IdentityDocumentCreate(IdentityDocumentCreate)
                                   , Schema(Schema)
                                   , SchemaDataType(SchemaDataType)
                                   , SchemaProperties(SchemaProperties)
                                   , DomainSuggestion(DomainSuggestion)
                                   , TldSummary(TldSummary)
                                   , DomainPurchase(DomainPurchase)
                                   ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Char
import           GHC.Generics

data AddressMailing = AddressMailing { address1   :: String
                                     , address2   :: Maybe String
                                     , city       :: String
                                     , state      :: String
                                     , postalCode :: String
                                     , country    :: String } deriving (Show)
$(deriveJSON defaultOptions ''AddressMailing)

data Contact = Contact { nameFirst      :: String
                       , nameMiddle     :: Maybe String
                       , nameLast       :: String
                       , organization   :: Maybe String
                       , jobTitle       :: Maybe String
                       , email          :: String
                       , phone          :: String
                       , fax            :: Maybe String
                       , addressMailing :: AddressMailing } deriving (Show)
$(deriveJSON defaultOptions ''Contact)

data Contacts = Contacts { registrant :: Contact
                         , admin      :: Maybe Contact
                         , tech       :: Maybe Contact
                         , billing    :: Maybe Contact } deriving (Show)

instance ToJSON Contacts where
  toJSON (Contacts r a t b) =
    object [ "contactRegistrant" .= r
           , "contactAdmin" .= a
           , "contactTech" .= t
           , "contactBilling" .= b ]

instance FromJSON Contacts where
  parseJSON (Object v) =
    Contacts <$> v .: "contactRegistrant"
             <*> v .:? "contactAdmin"
             <*> v .:? "contactTech"
             <*> v .:? "contactBilling"
  parseJSON _ = fail "Contacts object not found"

data RealNameValidation = RealNameValidation { r_status :: Maybe String } deriving (Show)

instance ToJSON RealNameValidation where
  toJSON (RealNameValidation s) =
    object [ "status" .= s ]

instance FromJSON RealNameValidation where
  parseJSON (Object v) =
    RealNameValidation <$> v .:? "status"
  parseJSON _ = fail "RealNameValidation object not found"

data DomainSummary = DomainSummary { domainId            :: Float
                                   , domain              :: String
                                   , status              :: String
                                   , expires             :: Maybe String
                                   , expirationProtected :: Bool
                                   , holdRegistar        :: Bool
                                   , locked              :: Bool
                                   , privacy             :: Bool
                                   , renewAuto           :: Bool
                                   , renewable           :: Bool
                                   , renewDeadline       :: String
                                   , transferProtected   :: Bool
                                   , createdAt           :: String
                                   , authCode            :: Maybe String
                                   , nameServers         :: Maybe [String]
                                   , contactRegistrant   :: Maybe Contact
                                   , contactBill         :: Maybe Contact
                                   , contactAdmin        :: Maybe Contact
                                   , contactTech         :: Maybe Contact
                                   , realNameValidation  :: Maybe RealNameValidation
                                   , subaccountId        :: Maybe String} deriving (Show)
$(deriveJSON defaultOptions ''DomainSummary)


data DomainUpdate = DomainUpdate { dlocked       :: Maybe Bool
                                 , dnameServers  :: Maybe [String]
                                 , drenewAuto    :: Maybe Bool
                                 , dsubaccountId :: Maybe String } deriving (Generic, Show)

instance ToJSON DomainUpdate where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON DomainUpdate where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

data Consent = Consent { agreementKeys :: [String]
                       , agreedBy      :: String
                       , agreedAt      :: String } deriving (Show)
$(deriveJSON defaultOptions ''Consent)

data PrivacyPurchase = PrivacyPurchase { consent :: Consent } deriving (Show)
$(deriveJSON defaultOptions ''PrivacyPurchase)


data DomainPurchaseResponse = DomainPurchaseResponse { orderId   :: Integer
                                                     , itemCount :: Integer
                                                     , total     :: Integer
                                                     , currency  :: String } deriving (Show)
$(deriveJSON defaultOptions ''DomainPurchaseResponse)

data DNSRecord = DNSRecord { recordType     :: String
                           , recordName     :: String
                           , recordData     :: Maybe String
                           , recordPriority :: Maybe String
                           , recordTTL      :: Maybe String
                           , recordService  :: Maybe String
                           , recordProtocol :: Maybe String
                           , recordPort     :: Maybe Integer
                           , recordWeight   :: Maybe Integer } deriving (Generic, Show)

instance ToJSON DNSRecord where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = (\x -> map toLower $ drop 6 x) }

instance FromJSON DNSRecord where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = (\x -> map toLower $ drop 6 x) }

data DomainRenew = DomainRenew { period :: Integer } deriving (Show)
$(deriveJSON defaultOptions ''DomainRenew)

data DomainTransferIn = DomainTransferIn { transAuthCode  :: String
                                         , transPeriod    :: Maybe Integer
                                         , transRenewAuto :: Maybe Bool
                                         , transPrivacy   :: Maybe Bool
                                         , transConsent   :: Consent } deriving (Generic, Show)

instance ToJSON DomainTransferIn where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = (\x -> (toLower $ head x):[] ++ (tail x)) . drop 5 }

instance FromJSON DomainTransferIn where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = (\x -> (toLower $ head x):[] ++ (tail x)) . drop 5 }

data LegalAgreement = LegalAgreement { agreementKey :: String
                                     , title        :: String
                                     , url          :: Maybe String
                                     , content      :: String } deriving (Show)
$(deriveJSON defaultOptions ''LegalAgreement)

data DomainAvailableResponse = DomainAvailableResponse { availDomain   :: String
                                                       , available     :: Bool
                                                       , price         :: Maybe Integer
                                                       , availCurrency :: Maybe String
                                                       , availPeriod   :: Maybe Integer} deriving (Show)

instance ToJSON DomainAvailableResponse where
  toJSON (DomainAvailableResponse av a p ac ap) =
    object [ "domain" .= av
           , "available" .= a
           , "price" .= p
           , "currency" .= ac
           , "period" .= ap ]

instance FromJSON DomainAvailableResponse where
  parseJSON (Object v) =
    DomainAvailableResponse <$> v .: "domain"
                            <*> v .: "available"
                            <*> v .:? "price"
                            <*> v .:? "currency"
                            <*> v .:? "period"
  parseJSON _ = fail "DomainAvailableResponse object not found"

data IdentityDocumentCreate = IdentityDocumentCreate { identificationType   :: String
                                                     , identityDomain       :: String
                                                     , legalEntityName      :: String
                                                     , identificationNumber :: String
                                                     , image                :: String
                                                     , identityConcent      :: Consent } deriving (Show)

instance ToJSON IdentityDocumentCreate where
  toJSON (IdentityDocumentCreate it id len idn i idc) =
    object [ "identificationType" .= it
           , "domain" .= id
           , "legalEntityName" .= len
           , "image" .= i
           , "consent" .= idc ]

instance FromJSON IdentityDocumentCreate where
  parseJSON (Object v) =
    IdentityDocumentCreate <$> v .: "identificationType"
                           <*> v .: "domain"
                           <*> v .: "legalEntityName"
                           <*> v .: "identificationNumber"
                           <*> v .: "iamge"
                           <*> v .: "concent"
  parseJSON _ = fail "IdentityDocumentCreate object not found"

data DomainPurchase = DomainPurchase { purchaseDomain      :: String
                                     , purchaseConsent     :: Consent
                                     , purchasePeriod      :: Maybe Integer
                                     , purchaseNameServers :: Maybe [String]
                                     , purchaseRenewAuto   :: Maybe Bool
                                     , purchasePrivacy     :: Maybe Bool
                                     , purchaseRegistrant  :: Maybe Contact
                                     , purchaseAdmin       :: Maybe Contact
                                     , purchaseTech        :: Maybe Contact
                                     , purchaseBilling     :: Maybe Contact } deriving (Show)

instance ToJSON DomainPurchase where
  toJSON (DomainPurchase pd pc pp pns pr ppr pre pa pt pb) =
    object [ "domain" .= pd
           , "consent" .= pc
           , "period" .= pp
           , "nameServers" .= pns
           , "renewAuto" .= pr
           , "privacy" .= ppr
           , "contactRegistrant" .= pre
           , "contactAdmin" .= pa
           , "contactTech" .= pt
           , "contactBilling" .= pb ]

instance FromJSON DomainPurchase where
  parseJSON (Object v) =
    DomainPurchase <$> v .: "domain"
                   <*> v .: "consent"
                   <*> v .:? "period"
                   <*> v .:? "nameServers"
                   <*> v .:? "renewAuto"
                   <*> v .:? "privacy"
                   <*> v .:? "contactRegistrant"
                   <*> v .:? "contactAdmin"
                   <*> v .:? "contactTech"
                   <*> v .:? "contactBilling"
  parseJSON _ = fail "DomainPurchase object not found"

data SchemaDataType = SchemaDataType { dataType    :: String
                                     , dataRef     :: String
                                     , dataFormat  :: Maybe String
                                     , dataPattern :: Maybe String } deriving (Show)

instance ToJSON SchemaDataType where
  toJSON (SchemaDataType dt dr df dp) =
    object [ "type" .= dt
           , "$ref" .= dr
           , "format" .= df
           , "pattern" .= dp ]

instance FromJSON SchemaDataType where
  parseJSON (Object v) =
    SchemaDataType <$> v .: "type"
                   <*> v .: "$ref"
                   <*> v .:? "format"
                   <*> v .:? "pattern"
  parseJSON _ = fail "SchemaDataType object not found"

data SchemaProperties = SchemaProperties { propertyType         :: String
                                         , propertyRef          :: String
                                         , propertyItems        :: Maybe [SchemaDataType]
                                         , propertyRequired     :: Bool
                                         , propertyMaxItems     :: Maybe Integer
                                         , propertyMinItems     :: Maybe Integer
                                         , propertyDefaultValue :: Maybe String
                                         , propertyFormat       :: Maybe String
                                         , propertyPattern      :: Maybe String
                                         , propertyMaximum      :: Maybe Integer
                                         , propertyMinimum      :: Maybe Integer } deriving (Show)

instance ToJSON SchemaProperties where
  toJSON (SchemaProperties pt pr pi preq pmax pmin pdv pf pp pm pn) =
    object [ "type" .= pt
           , "$ref" .= pr
           , "items" .= pi
           , "required" .= preq
           , "maxItems" .= pmax
           , "minItems" .= pmin
           , "defaultValue" .= pdv
           , "format" .= pf
           , "pattern" .= pp
           , "maximum" .= pm
           , "minimum" .= pn ]

instance FromJSON SchemaProperties where
  parseJSON (Object v) =
    SchemaProperties <$> v .: "type"
                     <*> v .: "$ref"
                     <*> v .:? "items"
                     <*> v .: "required"
                     <*> v .:? "maxItems"
                     <*> v .:? "minItems"
                     <*> v .:? "defaultValue"
                     <*> v .:? "format"
                     <*> v .:? "pattern"
                     <*> v .:? "maximum"
                     <*> v .:? "minimum"
  parseJSON _ = fail "SchemaProperties object not found"

data Schema = Schema { schemaId         :: String
                     , schemaProperties :: [SchemaProperties]
                     , schemaRequired   :: [String]
                     , schemaModels     :: Schema } deriving (Generic, Show)

instance ToJSON Schema where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = (\x -> map toLower $ drop 6 x) }

instance FromJSON Schema where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = (\x -> map toLower $ drop 6 x) }

data DomainSuggestion = DomainSuggestion { suggestedDomain :: String } deriving (Generic, Show)

instance ToJSON DomainSuggestion where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = (\x -> map toLower $ drop 9 x) }

instance FromJSON DomainSuggestion where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = (\x -> map toLower $ drop 9 x) }

data TldSummary = TldSummary { tldName :: String
                             , tldType :: String } deriving (Generic, Show)

instance ToJSON TldSummary where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = (\x -> map toLower $ drop 3 x) }

instance FromJSON TldSummary where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = (\x -> map toLower $ drop 3 x) }

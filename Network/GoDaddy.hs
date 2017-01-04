{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module       : Network.GoDaddy
-- Copyright   : (c) 2016 Kyle Gwinnup
--
-- License     : BSD-style
-- Maintainer  : kpgwinnup@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- A library for interacting with GoDaddy's REST API

module Network.GoDaddy ( GoDaddyAuth(GoDaddyAuth)
               , getDomains
               , updateDomainDetails
               , getDomain
               , setDomainContacts
               , cancelDomainPrivacy
               , purchaseDomainPrivacy
               , addDomainRecord
               , replaceAllDNSRecords
               , getDomainRecords
               , replaceAllDNSRecordsByType
               , replaceAllDNSRecordsByTypeAndName
               , renewDomain
               , startDomainTransfer
               , resendContactEmailVerification
               , getLegalAgreements
               , isDomainAvailable
               , updateIdentityDocument
               , purchaseDomain
               , getSchemaForTld
               , validatePurchaseSchema
               , suggestDomain
               , getTldsForSale
               , getAbuseTicket
               , getAbuseTickets
               , createAbuseTicket
               , deleteAftermarketListing
               , addAftermarketExpiryListing
               ) where

import           Control.Exception                as E
import           Control.Lens                     ((&), (.~), (^.))
import           Data.Aeson                       (FromJSON, decode)
import           Data.ByteString.Lazy             as L
import           Data.Text                        (Text)
import           Data.Text                        as T
import           Data.Text.Encoding               (encodeUtf8)
import           Network.GoDaddy.AbuseTypes
import           Network.GoDaddy.AftermarketTypes
import qualified Network.GoDaddy.AgreementTypes   as A
import           Network.GoDaddy.DomainTypes
import           Network.GoDaddy.ErrorTypes
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status        (statusCode)

-- | GoDaddy Authentication structure, takes key and secret key
--
-- > >>> let x = GoDaddyAuth <key> <secret>
--
data GoDaddyAuth = GoDaddyAuth { goDaddyKey    :: String
                               , goDaddySecret :: String } deriving (Show)


-- | Transform an authentication data type to a Text type. This value is then used in sendRequest to build http request headers.
--
packAuth :: GoDaddyAuth -> Text
packAuth (GoDaddyAuth k s) = T.pack $ "sso-key " ++ k ++ ":" ++ s

sendRequest :: GoDaddyAuth -> String -> String -> IO (Either GError L.ByteString)
sendRequest auth typ url = do
  manager <- newManager tlsManagerSettings
  initialRequest <- parseRequest url
  let request = initialRequest
         { method = encodeUtf8 $ T.pack typ
         , requestHeaders = [
             ("Authorization", encodeUtf8 $ packAuth auth)
             ]}
  response <- httpLbs request manager
  if statusCode (responseStatus response) == 200
    then return $ Right $ responseBody response
    else return $ Left $ extractGError (decode $ responseBody response)

extractGError :: Maybe GError -> GError
extractGError (Just e) = e
extractGError Nothing = GError (Just "000") (Just "Error decoding response json error message") Nothing Nothing Nothing

maybeE :: Maybe m -> String -> String -> Either GError m
maybeE (Just m) _ _ = Right m
maybeE Nothing c s = Left $ GError (Just c) (Just s) Nothing Nothing Nothing

errorOrTrue :: Either GError L.ByteString -> Either GError Bool
errorOrTrue (Left e) = Left e
errorOrTrue (Right _) = Right True

maybeDecode :: FromJSON a => Either GError L.ByteString -> String -> String -> Either GError a
maybeDecode (Left e) _ _ = Left e
maybeDecode (Right r) c m = maybeE (decode r) c m

maybeDecodeL :: FromJSON a => Either GError L.ByteString -> String -> String -> Either GError [a]
maybeDecodeL (Left e) _ _ = Left e
maybeDecodeL (Right r) c m = maybeE (decode r) c m


--
-- Domain methods
--

domainUrl = "https://api.godaddy.com/v1/domains"

-- | Retrieve a list of Domains for the specified Shopper
getDomains :: GoDaddyAuth -> IO (Either GError [DomainSummary])
getDomains auth = fmap (\x -> maybeDecode x "000" "error extracting DomainSummary") (sendRequest auth "GET" domainUrl )
-- | Update details for the specified Domain
updateDomainDetails :: GoDaddyAuth -> DomainUpdate -> IO (Either GError Bool)
updateDomainDetails auth du = fmap errorOrTrue (sendRequest auth "PATCH" domainUrl) 

-- | Cancel a purchased domain
--
-- > >>> deleteDomain auth "example.com"
deleteDomain :: GoDaddyAuth -- ^ godaddy auth
             -> String -- ^ domain name
             -> IO (Either GError Bool)
deleteDomain auth domain = fmap errorOrTrue (sendRequest auth "DELETE" (domainUrl ++ "/" ++ domain))

-- | Retrieve details for the specified Domain
--
-- > >>> getDomain auth "example.io"
getDomain :: GoDaddyAuth -- ^ godaddy auth
          -> String -- ^ domain name
          -> IO (Either GError DomainSummary)
getDomain auth domain = fmap (\x -> maybeDecode x "000" "error extracting DomainSummary") (sendRequest auth "GET" (domainUrl ++ "/" ++ domain))
-- | Update domain contacts
--
-- > >>> setDomainContacts auth "example.com" contacts
setDomainContacts :: GoDaddyAuth -- ^ godaddy auth
                  -> String -- ^ domain name
                  -> Contacts -- ^ contacts
                  -> IO (Either GError Bool)
setDomainContacts auth domain contacts = fmap errorOrTrue (sendRequest auth "PATCH" (domainUrl ++ "/" ++ domain ++ "/contacts"))

-- | Submit a privacy cancellation request for the given domain
--
-- > >>> cancelDomainPrivacy auth "example.com"
cancelDomainPrivacy :: GoDaddyAuth -- ^ godaddy auth
                    -> String -- ^ domain name
                    -> IO (Either GError Bool)
cancelDomainPrivacy auth domain = fmap errorOrTrue (sendRequest auth "DELETE" (domainUrl ++ "/" ++ domain ++ "/privacy"))

-- | Purchase privacy for a specified domain
--
-- >>> purchaseDomainPrivacy auth "example.com" privacyPuchase
purchaseDomainPrivacy :: GoDaddyAuth -- ^ godaddy auth
                      -> String -- ^ domain name
                      -> PrivacyPurchase -- ^ privacy purchase
                      -> IO (Either GError DomainPurchaseResponse)
purchaseDomainPrivacy auth domain privacy = fmap (\x -> maybeDecode x "000" "error extracting DomainPurchaseResponse") (sendRequest auth "POST" (domainUrl ++ "/" ++ domain ++ "/privacy/purchase"))

-- | Add the specified DNS Records to the specified Domain
addDomainRecord :: GoDaddyAuth -- ^ godaddy auth
                -> String -- ^ domain name
                -> DNSRecord -- ^ dnsrecord
                -> IO (Either GError Bool)
addDomainRecord auth domain record = fmap errorOrTrue (sendRequest auth "PATCH" (domainUrl ++ "/" ++ domain ++ "/records"))

-- | Replace all DNS Records for the specified Domain
replaceAllDNSRecords :: GoDaddyAuth -- ^ godaddy auth
                     -> String -- ^ domain name
                     -> [DNSRecord] -- ^ list of dns records
                     -> IO (Either GError Bool)
replaceAllDNSRecords auth domain records = fmap errorOrTrue (sendRequest auth "PUT" (domainUrl ++ "/" ++ domain ++ "/records"))

-- | Retrieve DNS Records for the specified Domain, optionally with the specified Type and/or Name
getDomainRecords :: GoDaddyAuth -- ^ godaddy auth
                 -> String -- ^ domain name
                 -> Maybe String -- ^ DNS Record Type for which DNS Records are to be retrieved
                 -> Maybe String -- ^ DNS Record Name for which DNS Records are to be retrieved
                 -> IO (Either GError [DNSRecord])
getDomainRecords auth domain (Just t) (Just n) = fmap (\x -> maybeDecodeL x "000" "error extracting DNSRecord") (sendRequest auth "GET" (domainUrl ++ "/" ++ domain ++ "/records/" ++ t ++ "/" ++ n))
getDomainRecords auth domain (Just t) _ = fmap (\x -> maybeDecodeL x "000" "error extracting DNSRecord") (sendRequest auth "GET" (domainUrl ++ "/" ++ domain ++ "/records/" ++ t)) 
getDomainRecords auth domain _ (Just n) = fmap (\x -> maybeDecodeL x "000" "error extracting DNSRecord") (sendRequest auth "GET" (domainUrl ++ "/" ++ domain ++ "/records/" ++ n)) 
getDomainRecords auth domain _ _ = fmap (\x -> maybeDecodeL x "000" "error etracting DNSRecord") (sendRequest auth "GET" (domainUrl ++ "/" ++ domain ++ "/records"))

-- | Replace all DNS Records for the specified Domain with the specified Type
replaceAllDNSRecordsByType :: GoDaddyAuth -- ^ godaddy auth
                           -> String -- ^ domain name
                           -> String -- ^ domain record type
                           -> IO (Either GError Bool)
replaceAllDNSRecordsByType auth domain typ = fmap errorOrTrue (sendRequest auth "PUT" (domainUrl ++ "/" ++ domain ++ "/records/" ++ typ))

-- | Replace all DNS Records for the specified Domain with the specified Type and Name
replaceAllDNSRecordsByTypeAndName :: GoDaddyAuth -- ^ godaddy auth
                                  -> String -- ^ domain name
                                  -> String -- ^ DNS record type
                                  -> String -- ^ DNS record name
                                  -> IO (Either GError Bool)
replaceAllDNSRecordsByTypeAndName auth domain typ name = fmap errorOrTrue (sendRequest auth "PUT" (domainUrl ++ "/" ++ domain ++ "/records/" ++ typ ++ "/" ++ name))

-- | Renew the specified Domain
renewDomain :: GoDaddyAuth -- ^ godaddy auth
            -> String -- ^ domain name
            -> IO (Either GError DomainPurchaseResponse)
renewDomain auth domain = fmap (\x -> maybeDecode x "000" "error extracting DomainPurchaseResponse") (sendRequest auth "POST" (domainUrl ++ "/" ++ domain ++ "/renew"))

-- | Purchase and start or restart transfer process
startDomainTransfer :: GoDaddyAuth -- ^ godaddy auth
                    -> String -- ^ domain name
                    -> IO (Either GError DomainTransferIn)
startDomainTransfer auth domain = fmap (\x -> maybeDecode x "000" "error extracting DomainPurchaseResponse") (sendRequest auth "POST" (domainUrl ++ "/" ++ domain ++ "/transfer")) 

-- | Re-send Contact E-mail Verification for specified Domain
resendContactEmailVerification :: GoDaddyAuth -- ^ godaddy auth
                               -> String -- ^ domain name
                               -> IO (Either GError Bool)
resendContactEmailVerification auth domain = fmap errorOrTrue (sendRequest auth "POST" (domainUrl ++ "/" ++ domain ++ "/verifyRegistrantEmail"))

-- | Retrieve the legal agreement(s) required to purchase the specified TLD and add-ons
getLegalAgreements :: GoDaddyAuth -> IO (Either GError [LegalAgreement])
getLegalAgreements auth = fmap (\x -> maybeDecodeL x "000" "error extracting LegalAgreements") (sendRequest auth "GET" (domainUrl ++ "/agreements")) 

-- | Determine whether or not the specified domain is available for purchase
isDomainAvailable :: GoDaddyAuth -- ^ godaddy auth
                  -> String -- ^ domain name
                  -> IO (Either GError DomainAvailableResponse)
isDomainAvailable auth domain = fmap (\x -> maybeDecode x "000" "error extracting DomainAvailableResponse") (sendRequest auth "GET" (domainUrl ++ "/available/?domain=" ++ domain)) 

-- | Upload an identity document for Real Name Validation
updateIdentityDocument :: GoDaddyAuth
                       -> IdentityDocumentCreate
                       -> IO (Either GError Bool)
updateIdentityDocument auth doc = fmap errorOrTrue (sendRequest auth "POST" (domainUrl ++ "/identityDocuments"))

-- | Purchase and register the specified Domain
purchaseDomain :: GoDaddyAuth -> DomainPurchase -> IO (Either GError DomainPurchaseResponse)
purchaseDomain auth domain = fmap (\x -> maybeDecode x "000" "error extracting DomainPurchaseResponse") (sendRequest auth "POST" (domainUrl ++ "/purchase")) 

-- | Retrieve the schema to be submitted when registering a Domain for the specified TLD
getSchemaForTld :: GoDaddyAuth -- ^ godaddy auth
                -> String -- ^ The Top-Level Domain whose schema should be retrieved
                -> IO (Either GError Schema)
getSchemaForTld auth tld = fmap (\x -> maybeDecode x "000" "error extracting Schema") (sendRequest auth "GET" (domainUrl ++ "/purchase/schema/" ++ tld)) 

-- | Validate the request body using the Domain Purchase Schema for the specified TLD
validatePurchaseSchema :: GoDaddyAuth -> DomainPurchase -> IO (Either GError Bool)
validatePurchaseSchema auth schema = fmap errorOrTrue (sendRequest auth "POST" (domainUrl ++ "/purchase/validate"))

-- | Suggest alternate Domain names based on a seed Domain or set of keywords
suggestDomain :: GoDaddyAuth -- ^ godaddy auth
              -> String  -- ^ Domain name or set of keywords for which alternative domain names will be suggested
              -> IO (Either GError [DomainSuggestion])
suggestDomain auth keywords = fmap (\x -> maybeDecodeL x "000" "error extracting DomainSuggestion") (sendRequest auth "GET" (domainUrl ++ "/suggest")) 

-- | Retrieves a list of TLDs supported and enabled for sale
getTldsForSale :: GoDaddyAuth -> IO (Either GError [TldSummary])
getTldsForSale auth = fmap (\x -> maybeDecodeL x "000" "error exracting TldSummary") (sendRequest auth "GET" (domainUrl ++ "/tlds")) 

--
-- Abuse
--

abuseUrl = "https://api.godaddy.com/v1/abuse"

-- | List all abuse tickets ids that match user provided filters
getAbuseTickets :: GoDaddyAuth -> IO (Either GError AbuseTicketList)
getAbuseTickets auth = fmap (\x -> maybeDecode x "000" "error extracting AbuseTicketList") (sendRequest auth "GET" (abuseUrl ++ "/tickets")) 

-- | Create a new abuse ticket
createAbuseTicket :: GoDaddyAuth -> AbuseTicketCreate -> IO (Either GError AbuseTicketId)
createAbuseTicket auth ticket = fmap (\x -> maybeDecode x "000" "error extracting AbuseTicketId") (sendRequest auth "POST" (abuseUrl ++ "/tickets")) 

-- | Return the abuse ticket data for a given ticket id
getAbuseTicket :: GoDaddyAuth -- ^ godaddy auth
               -> Integer -- ^ ticket id
               -> IO (Either GError AbuseTicket)
getAbuseTicket auth id = fmap (\x -> maybeDecode x "000" "error extracting AbuseTicket") (sendRequest auth "GET" (abuseUrl ++ "/tickets/" ++ show id))

--
-- Aftermarket
--

aftermarketUrl = "https://api.godaddy.com/v1/aftermarket"

-- | Remove listings from GoDaddy Auction
deleteAftermarketListing :: GoDaddyAuth -- ^ godaddy auth
                         -> String -- ^ A comma separated list of domain names
                         -> IO (Either GError AftermarketListingAction)
deleteAftermarketListing auth domains = fmap (\x -> maybeDecode x "000" "error extracting ListingAction") (sendRequest auth "DELETE" (aftermarketUrl ++ "/listing/" ++ domains)) 

-- | Add expiry listings into GoDaddy Auction
addAftermarketExpiryListing :: GoDaddyAuth -> [AftermarketListingExpiryCreate] -> IO (Either GError AftermarketListingAction)
addAftermarketExpiryListing auth listings = 
    fmap (\x -> maybeDecode x "000" "error extracting AftermarketListingAction") (sendRequest auth "POST" (aftermarketUrl ++ "/listing/expiry"))

--
-- Agreements
--

agreementsUrl = "https://api.godaddy.com/v1/agreements"

-- | Retrieve Legal Agreements for provided agreements keys
getLegalAgreementsForKeys :: GoDaddyAuth -- ^ godaddy auth
                          -> String -- ^ Keys for Agreements whose details are to be retrieved
                          -> IO (Either GError [A.LegalAgreement])
getLegalAgreementsForKeys auth keys = 
    fmap (\x -> maybeDecodeL x "000" "error extracting LegalAgreement") (sendRequest auth "GET" agreementsUrl)

--
-- Cart
--

cartUrl = "https://api.godaddy.com/v1/cart"


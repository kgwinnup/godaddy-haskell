
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
               ) where

import           Control.Exception           as E
import           Control.Lens                ((&), (.~), (^.))
import           Data.Aeson                  (FromJSON, decode)
import           Data.ByteString.Lazy        as L
import           Data.Text                   (Text)
import           Data.Text                   as T
import           Data.Text.Encoding          (encodeUtf8)
import           Network.GoDaddy.DomainTypes
import           Network.GoDaddy.ErrorTypes
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status   (statusCode)

data GoDaddyAuth = GoDaddyAuth String String deriving (Show)

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
  if (statusCode $ responseStatus response) == 200
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

maybeDecode :: FromJSON a => (Either GError L.ByteString) -> String -> String -> (Either GError a)
maybeDecode (Left e) _ _ = Left e
maybeDecode (Right r) c m = maybeE (decode r) c m

maybeDecodeL :: FromJSON a => (Either GError L.ByteString) -> String -> String -> (Either GError [a])
maybeDecodeL (Left e) _ _ = Left e
maybeDecodeL (Right r) c m = maybeE (decode r) c m

--
-- Domain methods
--

rootUrl = "https://api.godaddy.com/v1/domains"

getDomains :: GoDaddyAuth -> IO (Either GError [DomainSummary])
getDomains auth = sendRequest auth "GET" rootUrl >>= return . (\x -> maybeDecode x "000" "error extracting DomainSummary")

updateDomainDetails :: GoDaddyAuth -> DomainUpdate -> IO (Either GError Bool)
updateDomainDetails auth du = sendRequest auth "PATCH" rootUrl >>= return . errorOrTrue

deleteDomain :: GoDaddyAuth -> String -> IO (Either GError Bool)
deleteDomain auth domain = sendRequest auth "DELETE" (rootUrl ++ "/" ++ domain) >>= return . errorOrTrue

getDomain :: GoDaddyAuth -> String -> IO (Either GError DomainSummary)
getDomain auth domain = sendRequest auth "GET" (rootUrl ++ "/" ++ domain) >>= return . (\x -> maybeDecode x "000" "error extracting DomainSummary")

setDomainContacts :: GoDaddyAuth -> String -> Contacts -> IO (Either GError Bool)
setDomainContacts auth domain contacts = sendRequest auth "PATCH" (rootUrl ++ "/" ++ domain ++ "/contacts") >>= return . errorOrTrue

cancelDomainPrivacy :: GoDaddyAuth -> String -> IO (Either GError Bool)
cancelDomainPrivacy auth domain = sendRequest auth "DELETE" (rootUrl ++ "/" ++ domain ++ "/privacy") >>= return . errorOrTrue

purchaseDomainPrivacy :: GoDaddyAuth -> String -> PrivacyPurchase -> IO (Either GError DomainPurchaseResponse)
purchaseDomainPrivacy auth domain privacy = sendRequest auth "POST" (rootUrl ++ "/" ++ domain ++ "/privacy/purchase") >>= return . (\x -> maybeDecode x "000" "error extracting DomainPurchaseResponse")

addDomainRecord :: GoDaddyAuth -> String -> DNSRecord -> IO (Either GError Bool)
addDomainRecord auth domain record = sendRequest auth "PATCH" (rootUrl ++ "/" ++ domain ++ "/records") >>= return . errorOrTrue

replaceAllDNSRecords :: GoDaddyAuth -> String -> [DNSRecord] -> IO (Either GError Bool)
replaceAllDNSRecords auth domain records = sendRequest auth "PUT" (rootUrl ++ "/" ++ domain ++ "/records") >>= return . errorOrTrue

getDomainRecords :: GoDaddyAuth -> String -> Maybe String -> Maybe String -> IO (Either GError [DNSRecord])
getDomainRecords auth domain (Just t) (Just n) = sendRequest auth "GET" (rootUrl ++ "/" ++ domain ++ "/records/" ++ t ++ "/" ++ n) >>= return . (\x -> maybeDecodeL x "000" "error extracting DNSRecord")
getDomainRecords auth domain (Just t) _ = sendRequest auth "GET" (rootUrl ++ "/" ++ domain ++ "/records/" ++ t) >>= return . (\x -> maybeDecodeL x "000" "error extracting DNSRecord")
getDomainRecords auth domain _ (Just n) = sendRequest auth "GET" (rootUrl ++ "/" ++ domain ++ "/records/" ++ n) >>= return . (\x -> maybeDecodeL x "000" "error extracting DNSRecord")
getDomainRecords auth domain _ _ = sendRequest auth "GET" (rootUrl ++ "/" ++ domain ++ "/records") >>= return . (\x -> maybeDecodeL x "000" "error etracting DNSRecord")

replaceAllDNSRecordsByType :: GoDaddyAuth -> String -> String -> IO (Either GError Bool)
replaceAllDNSRecordsByType auth domain typ = sendRequest auth "PUT" (rootUrl ++ "/" ++ domain ++ "/records/" ++ typ) >>= return . errorOrTrue

replaceAllDNSRecordsByTypeAndName :: GoDaddyAuth -> String -> String -> String -> IO (Either GError Bool)
replaceAllDNSRecordsByTypeAndName auth domain typ name = sendRequest auth "PUT" (rootUrl ++ "/" ++ domain ++ "/records/" ++ typ ++ "/" ++ name) >>= return . errorOrTrue

renewDomain :: GoDaddyAuth -> String -> IO (Either GError DomainPurchaseResponse)
renewDomain auth domain = sendRequest auth "POST" (rootUrl ++ "/" ++ domain ++ "/renew") >>= return . (\x -> maybeDecode x "000" "error extracting DomainPurchaseResponse")

startDomainTransfer :: GoDaddyAuth -> String -> IO (Either GError DomainTransferIn)
startDomainTransfer auth domain = sendRequest auth "POST" (rootUrl ++ "/" ++ domain ++ "/transfer") >>= return . (\x -> maybeDecode x "000" "error extracting DomainPurchaseResponse")

resendContactEmailVerification :: GoDaddyAuth -> String -> IO (Either GError Bool)
resendContactEmailVerification auth domain = sendRequest auth "POST" (rootUrl ++ "/" ++ domain ++ "/verifyRegistrantEmail") >>= return . errorOrTrue

getLegalAgreements :: GoDaddyAuth -> IO (Either GError [LegalAgreement])
getLegalAgreements auth = sendRequest auth "GET" (rootUrl ++ "/agreements") >>= return . (\x -> maybeDecodeL x "000" "error extracting LegalAgreements")

isDomainAvailable :: GoDaddyAuth -> String -> IO (Either GError DomainAvailableResponse)
isDomainAvailable auth domain = sendRequest auth "GET" (rootUrl ++ "/available/?domain=" ++ domain) >>= return . (\x -> maybeDecode x "000" "error extracting DomainAvailableResponse")

updateIdentityDocument :: GoDaddyAuth -> IdentityDocumentCreate -> IO (Either GError Bool)
updateIdentityDocument auth doc = sendRequest auth "POST" (rootUrl ++ "/identityDocuments") >>= return . errorOrTrue

purchaseDomain :: GoDaddyAuth -> DomainPurchase -> IO (Either GError DomainPurchaseResponse)
purchaseDomain auth domain = sendRequest auth "POST" (rootUrl ++ "/purchase") >>= return . (\x -> maybeDecode x "000" "error extracting DomainPurchaseResponse")

getSchemaForTld :: GoDaddyAuth -> String -> IO (Either GError Schema)
getSchemaForTld auth tld = sendRequest auth "GET" (rootUrl ++ "/purchase/schema/" ++ tld) >>= return . (\x -> maybeDecode x "000" "error extracting Schema")

validatePurchaseSchema :: GoDaddyAuth -> DomainPurchase -> IO (Either GError Bool)
validatePurchaseSchema auth schema = sendRequest auth "POST" (rootUrl ++ "/purchase/validate") >>= return . errorOrTrue

suggestDomain :: GoDaddyAuth -> String -> IO (Either GError [DomainSuggestion])
suggestDomain auth keywords = sendRequest auth "GET" (rootUrl ++ "/suggest") >>= return . (\x -> maybeDecodeL x "000" "error extracting DomainSuggestion")

getTldsForSale :: GoDaddyAuth -> IO (Either GError [TldSummary])
getTldsForSale auth = sendRequest auth "GET" (rootUrl ++ "/tlds") >>= return . (\x -> maybeDecodeL x "000" "error exracting TldSummary")


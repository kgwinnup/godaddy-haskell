
{-# LANGUAGE OverloadedStrings #-}

module GoDaddy ( GoDaddyAuth(GoDaddyAuth)
               , packAuth
               , sendRequest
               , maybeE
               , errorOrTrue
               , maybeDecode
               , maybeDecodeL
               ) where

import           Control.Exception         as E
import           Control.Lens              ((&), (.~), (^.))
import           Data.Aeson                (decode, FromJSON)
import           Data.ByteString.Lazy      as L
import           Data.Text                 (Text)
import           Data.Text                 as T
import           Data.Text.Encoding        (encodeUtf8)
import           DomainTypes
import           ErrorTypes
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status (statusCode)

data GoDaddyAuth = GoDaddyAuth String String deriving (Show)

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





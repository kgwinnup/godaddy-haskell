
Haskell bindings to the Godaddy API


```
module Main where

import           Control.Monad
import           Network.GoDaddy.DomainTypes
import           Network.GoDaddy.ErrorTypes
import           Network.GoDaddy
import           System.IO

main :: IO ()
main = do
  handle <- openFile "godaddy_settings.txt" ReadMode
  (k:s:_) <- hGetContents handle >>= return . words
  let auth = GoDaddyAuth k s
      up = DomainUpdate Nothing Nothing Nothing Nothing
  res <- getDomain auth "switchport.io"
  case res of
    Left e -> putStrLn $ show e
    Right ds -> putStrLn $ show ds
  print $ res

  res2 <- isDomainAvailable auth "switchport.io"
  case res2 of
    Left e -> putStrLn $ show e
    Right r -> putStrLn $ show r

  res3 <- getTldsForSale auth
  case res3 of
    Left e -> putStrLn $ show e
    Right r -> putStrLn $ show r
```

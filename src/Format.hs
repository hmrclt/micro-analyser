{-# LANGUAGE OverloadedStrings #-}

module Format where

import Control.Lens
import Control.Applicative
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import System.IO
import System.Exit (exitFailure)
import Data.Text (unpack, Text)
import Data.Maybe (fromJust, fromMaybe)
import Text.Regex
import Data.List (isPrefixOf)


resToOpt :: Result a -> Maybe a
resToOpt (Success a) = Just a
resToOpt (Error _) = Nothing


fromBuckets :: [Value] -> [([String], Integer)]
fromBuckets ks = revKeys <$> fromBuckets' [] ks
  where revKeys (a,b) = (reverse a, b)

f :: Integer -> Text
f = undefined

fromBuckets' :: [String] -> [Value] -> [([String], Integer)]
fromBuckets' prefix (json:xs) = inner ++ fromBuckets' prefix xs
  where
    inner :: [([String], Integer)]
    inner = case subbuckets of
      [] -> [( keyV : prefix, countV )]
      ys -> fromBuckets' (keyV : prefix) ys
    subbuckets = json ^.. members . key "buckets" . values
    keyV :: String
    keyV = fromJust $ keyVa <|> keyVb
    keyVa :: Maybe String
    keyVa = fmap show $ json ^? key "key" . _Integer
    keyVb :: Maybe String
    keyVb = unpack <$> json ^? key "key" . _String    
    countV :: Integer
    countV = fromMaybe 0 $ json ^? key "doc_count" . _Integer
fromBuckets' _ [] = []

debucket :: Value -> [Value]
debucket json = json  ^.. key "responses" . nth 0 . key "aggregations" . key "2" . key "buckets" . values

applySeq :: [a -> a] -> a -> a
applySeq fx el = foldl (\ i f -> f i) el fx

stripVars :: String -> String
stripVars = applySeq regMap
  where
    regexes = [ ("HTTP/.*", "")
              , ("/[0-9a-f]+([/? ])", "/{id}\\1")
              , ("[?][^ ]+", "")
              , ("/[a-zA-Z]{2}[0-9]{6}[A-D]?", "/{utr}")
              , ("/[0-9A-Fa-f]{8}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{12}?([/ ])", "/{uuid}")              
              , (" +$", "")              
              ]
    regMap = fmap (\ (f,t) x -> subRegex (mkRegex f) x t) regexes

removePublicUserAgents :: String -> String
removePublicUserAgents "AHC/2.0" = "Unknown (AHC)"
removePublicUserAgents ua | "Mozilla/" `isPrefixOf` ua = "Browser"
removePublicUserAgents ua | "HMRCNextGenConsumer/" `isPrefixOf` ua = "HMRCNextGenConsumer"
removePublicUserAgents ua = ua

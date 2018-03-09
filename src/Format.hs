{-# LANGUAGE OverloadedStrings #-}

module Format where

import Control.Lens
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy as B
import Data.Aeson (FromJSON(..), fromJSON, withObject, (.:))
import Data.Aeson (Result(..), Value)
import System.IO
import System.Exit (exitFailure)
import Data.Text (unpack)
import Data.Maybe (fromJust)
import Text.Regex

data Bucket = Bucket { _key :: String, _doc_count :: Integer }

class Csv a where
  toCsv :: a -> [String]

instance FromJSON Bucket where
  parseJSON = withObject "Bucket" $ \v -> Bucket
        <$> v .: "key"
        <*> v .: "doc_count" 

instance Csv Bucket where
  toCsv (Bucket k v) = [k, show v]

instance Show Bucket where
  show (Bucket k v) = "| " ++ k ++ "\t|" ++ show v ++ "\t|"

resToOpt :: Result a -> Maybe a
resToOpt (Success a) = Just a
resToOpt (Error _) = Nothing

unpackBuckets :: Maybe (Result [Bucket]) -> [Bucket]
unpackBuckets (Just (Success bx)) = bx
unpackBuckets _ = []

parseBuckets :: B.ByteString -> [Bucket]
parseBuckets bs = unpackBuckets fj
  where lensit = bs ^? key "responses" . nth 0 . key "aggregations" . key "2" . key "buckets"
        fj = fmap fromJSON lensit :: Maybe (Result [Bucket])

fromBuckets :: [Value] -> [([String], Integer)]
fromBuckets ks = fmap revKeys $ fromBuckets' [] ks
  where revKeys (a,b) = (reverse a, b)

fromBuckets' :: [String] -> [Value] -> [([String], Integer)]
fromBuckets' prefix (json:xs) = inner ++ fromBuckets' prefix xs
  where
    inner :: [([String], Integer)]
    inner = case (subbuckets) of
      [] -> [( keyV : prefix, countV )]
      ys -> fromBuckets' (keyV : prefix) ys
    subbuckets = json ^.. members . key "buckets" . values
    keyV ::  String
    keyV = unpack $ fromJust $ json ^? key "key" . _String
    countV ::  Integer
    countV = fromJust $ json ^? key "doc_count" . _Integer
fromBuckets' _ [] = []

valueToBuckets :: Value -> [Result Bucket]
valueToBuckets v = fmap fromJSON group
  where group = v ^.. key "responses" . nth 0 . key "aggregations" . key "2" . key "buckets" . values 
  
printBuckets :: [Result Bucket] -> IO ()
printBuckets [] = return ()
printBuckets (Success b:xs) = do
  print b
  printBuckets xs
printBuckets (Error e:_) = do
  (hPutStrLn stderr) e
  exitFailure

debucket :: Value -> [Value]
debucket json = json  ^.. key "responses" . nth 0 . key "aggregations" . key "2" . key "buckets" . values

applySeq :: [a -> a] -> a -> a
applySeq [] i = i
applySeq (f:fx) i = applySeq fx (f i)

stripVars :: String -> String
stripVars i = applySeq regMap i
  where
    regexes = [ ("HTTP/.*", "")
              , ("/[0-9a-f]+([/? ])", "/{id}\\1")
              , ("[?][^ ]+", "")
              , ("/[a-zA-Z]{2}[0-9]{6}[A-D]?", "/{utr}")
              , ("/[0-9A-Fa-f]{8}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{12}?([/ ])", "/{uuid}")              
              , (" +$", "")              
              ]
    regMap = fmap (\ (f,t) -> (\x -> subRegex (mkRegex f) x t)) regexes

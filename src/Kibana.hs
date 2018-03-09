{-# LANGUAGE OverloadedStrings #-}

module Kibana where

import Network.HTTP.Conduit
import MicroProbeOptions
import qualified Data.ByteString as B
import Network.HTTP.Types.Header (RequestHeaders)
import           Data.Aeson
import           Network.HTTP.Simple
import qualified Data.ByteString.Lazy as BL
import Data.Vector (fromList)
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format (formatTime)
import Format

import System.Directory (getHomeDirectory)
kibanaUrl :: Environment -> String
kibanaUrl Prod    = "https://kibana.tools.production.tax.service.gov.uk/elasticsearch/_msearch"
kibanaUrl _    = "https://postman-echo.com/post"
-- kibanaUrl _       = error "undefined environment"

bodyHeader' :: Value
bodyHeader' = object [ "index" .= [ String "logstash-*"], "ignore_unavailable" .= True, "preference" .= String "" ]

timeToValue :: UTCTime -> Value
timeToValue = toJSON . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

dateRangeValue :: UTCTime -> UTCTime -> Value
dateRangeValue from to  = object [ "range" .= object [
                            "@timestamp" .= object [
                                "gte" .= timeToValue from,
                                  "lte" .= timeToValue to,
                                  "format" .= String "date_hour_minute_second"]]]

searchQuery' :: Command  -> UTCTime -> UTCTime -> Value
searchQuery' (WhatTalksTo s) from to = object [ "query" .= object [
                                          "bool" .= object [
                                              "must" .= [ object [ "query_string" .= object [
                                                                   "query" .= query,
                                                                   "analyze_wildcard" .= True ]],
                                                          dateRangeValue from to
                                                        ],
                                                "must_not" .= (Array $ fromList [])
                                              ] ],
                                        "size" .= Number 0,
                                        "_source" .= object ["excludes" .= (Array $ fromList [])],
                                        "aggs" .= object [ "2" .= object ["terms" .= object [
                                                                             "field" .= String "http_user_agent.raw",
                                                                             "size" .= Number 500,
                                                                             "order" .= object ["_count" .= String "desc"]
                                                                             ],
                                                                          "aggs" .= object ["3" .= object [
                                                                                               "terms" .= object [
                                                                                                   "field" .= String "request.raw",
                                                                                                   "size" .= Number 50,
                                                                                                   "order" .= object ["_count" .= String "desc"]]]] ]
                                      ]]
  where query = "request: (\"GET /" ++ s ++ "/*\" OR \"POST /" ++ s ++ "/*\")"

combinedQuery :: Command -> UTCTime -> UTCTime -> BL.ByteString
combinedQuery c from to = BL.concat [ encode bodyHeader', "\n", encode (searchQuery' c from to), "\n" ]

dateRange :: DateOpts -> IO (UTCTime, UTCTime)
dateRange Today = do
  now <- getCurrentTime
  let start = UTCTime (utctDay now) (secondsToDiffTime 0)
  return (start, now)
dateRange (Between f Nothing) = do
  now <- getCurrentTime
  return (f, now)
dateRange (Between f (Just t)) = pure (f,t)
dateRange (On day) = pure (start,end)
  where start = UTCTime day (secondsToDiffTime 0)
        end   = UTCTime (addDays 1 $ day) (secondsToDiffTime 0)  

execQueryB' :: Value -> Environment -> Mode -> IO BL.ByteString
execQueryB' jsonQuery env mode = do
  let body = BL.concat [ encode bodyHeader', "\n", encode jsonQuery, "\n" ]
  secret <- readSecret
  initReq <- parseRequest $ kibanaUrl env
  let req = initReq { method = "POST"
                    , secure = True
                    , requestHeaders = (requestHeaders initReq) ++ extraHeaders secret
                    , requestBody = RequestBodyLBS $ body }
  getResponseBody <$> httpLBS req
  where
    extraHeaders :: B.ByteString -> RequestHeaders
    extraHeaders s = [ ("Authorization", B.concat ["Basic ",s])
                     , ("kbn-xsrf","reporting")
                     , ("content-type","application/x-ndjson") ]

execQuery' :: Value -> Environment -> IO Value
execQuery' jsonQuery env = do
  let body = BL.concat [ encode bodyHeader', "\n", encode jsonQuery, "\n" ]
  secret <- readSecret
  initReq <- parseRequest $ kibanaUrl env
  let req = initReq { method = "POST"
                    , secure = True
                    , requestHeaders = (requestHeaders initReq) ++ extraHeaders secret
                    , requestBody = RequestBodyLBS $ body }
  response <- httpJSON req
  return $ getResponseBody response
  -- BL.putStrLn $ encode (getResponseBody response :: Value)
  where
    extraHeaders :: B.ByteString -> RequestHeaders
    extraHeaders s = [ ("Authorization", B.concat ["Basic ",s])
                     , ("kbn-xsrf","reporting")
                     , ("content-type","application/x-ndjson") ]

readSecret :: IO B.ByteString
readSecret = do
  homeDir <- getHomeDirectory
  B.readFile $ homeDir ++ "/.micro-analyser/secret"

execQuery :: Options -> IO [Bucket]
execQuery (Options command env dateOpts _) = do
  (from, to) <- dateRange dateOpts
  secret <- readSecret
  initReq <- parseRequest $ kibanaUrl env
  let req = initReq { method = "POST"
                    , secure = True
                    , requestHeaders = (requestHeaders initReq) ++ extraHeaders secret
                    , requestBody = RequestBodyLBS $ combinedQuery command from to }
--  manager <- newManager tlsManagerSettings
  response <- httpLBS req
  return $ (parseBuckets . getResponseBody) response
  -- BL.putStrLn $ encode (getResponseBody response :: Value)
  where
    extraHeaders :: B.ByteString -> RequestHeaders
    extraHeaders s = [ ("Authorization", B.concat ["Basic ",s])
                     , ("kbn-xsrf","reporting")
                     , ("content-type","application/x-ndjson") ]

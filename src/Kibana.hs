{-# LANGUAGE OverloadedStrings #-}

module Kibana (dateRange, searchQuery', execQuery') where

import Network.HTTP.Conduit
import MicroProbeOptions
import qualified Data.ByteString as B
import Network.HTTP.Types.Header (RequestHeaders)
import           Data.Aeson
import           Network.HTTP.Simple
import qualified Data.ByteString.Lazy as BL
import Data.Vector (fromList)
import Data.Time
import Data.Time.Format (formatTime)
import qualified Data.Text as T
import Data.Maybe (catMaybes)

import System.Directory (getHomeDirectory)
kibanaUrl :: Environment -> String
kibanaUrl Prod    = "https://kibana.tools.production.tax.service.gov.uk/elasticsearch/_msearch"
kibanaUrl _    = "https://postman-echo.com/post"

bodyHeader' :: Value
bodyHeader' = object [ "index" .= [ String "logstash-*"], "ignore_unavailable" .= True, "preference" .= String "" ]

timeToValue :: UTCTime -> Value
timeToValue = toJSON . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

dateRangeValue :: UTCTime -> UTCTime -> Value
dateRangeValue from to  = object [
  "range" .= object [
      "@timestamp" .= object [
          "gte" .= timeToValue from,
            "lte" .= timeToValue to,
            "format" .= String "date_hour_minute_second"]]]

aggs :: [String] -> Value
aggs fields = inner fields (2 :: Integer)
  where
    inner [] _ = Null
    inner (field:fx) i =
      object [ (T.pack . show) i .= object (
                 "terms" .= object [
                      "field" .= String (T.pack field),
                        "size" .= Number 500,
                        "order" .= object ["_count" .= String "desc"]
                      ] : cont)
             ]
      where cont = case fx of
                     [] -> []
                     _ -> ["aggs" .= inner fx (i+1)]

searchQuery' :: Command  -> UTCTime -> UTCTime -> Value
searchQuery' (WhatTalksTo s breakdownEndPoints breakdownResponseCode) from to =
  object [ "query" .= object [
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
           "aggs" .= aggs aggParts]
  where query = "server_name: \"" ++ s ++ ".service\""
        aggParts = catMaybes [ Just "http_user_agent.raw"
                   , if breakdownEndPoints then Just "request.raw" else Nothing
                   , if breakdownResponseCode then Just "status" else Nothing
                   ]

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
        end   = UTCTime (addDays 1 day) (secondsToDiffTime 0)

execQuery' :: Value -> Environment -> Int -> IO Value
execQuery' jsonQuery env timeout = do
  let body = BL.concat [ encode bodyHeader', "\n", encode jsonQuery, "\n" ]
  secret <- readSecret
  initReq <- parseRequest $ kibanaUrl env
  let req = initReq { method = "POST"
                    , secure = True
                    , responseTimeout = responseTimeoutMicro $ timeout * 1000000
                    , requestHeaders = requestHeaders initReq ++ extraHeaders secret
                    , requestBody = RequestBodyLBS body
                    }
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

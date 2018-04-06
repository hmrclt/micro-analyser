{-# LANGUAGE OverloadedStrings #-}

module Test where

import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Format

jsonFile :: String -> IO Value
jsonFile filename = do
  bdata <- B.readFile filename
  let maybejson = eitherDecode bdata :: Either String Value
  case (maybejson) of
    Left e -> error $ "Unable to parse" ++ e
    Right m -> return m

bigOutput :: IO Value
bigOutput = jsonFile "../output.json"

denest :: Value
denest = case (eitherDecode text)
              of Left e -> error e
                 Right r -> r
  where text = "{ \"outer\": { \"one\" : [1,2,3], \"two\": [4,5,6]}}"


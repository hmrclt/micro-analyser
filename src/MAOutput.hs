module MAOutput ( jsonToTable, jsonToTable' ) where

import Format (fromBuckets, debucket, removePublicUserAgents)
import qualified Data.Map as M
import Data.Aeson
import Table
import Repo
import MicroProbeOptions
import Data.Maybe (catMaybes)

jsonToTable :: Value -> [String] -> Table String String
jsonToTable jsonV titles = table (titles++["Qty"]) $ fmap inlineQty b
  where
    b = M.toList (M.fromListWith (+) $ fmap (line []) dataOut)
    dataOut = fromBuckets $ debucket jsonV

jsonToTable' :: Value -> Command -> IO (Table String String)
jsonToTable' jsonV (WhatTalksTo rName ra rb) = do
  endpoints <- repoFromName rName >>= repoEndpoints
  return $ table titles $ fmap inlineQty (b endpoints)
  where
    b eps = M.toList (M.fromListWith (+) $ fmap (line eps) dataOut)
    dataOut = fromBuckets $ debucket jsonV
    titles = catMaybes [ Just "Caller"
                       , if ra then Just "Endpoint" else Nothing
                       , if rb then Just "Response" else Nothing
                       , Just "Qty"
                       ]
jsonToTable' jsonV _ = return $ jsonToTable jsonV []

line :: [Endpoint] -> ([String], Integer) -> ([String], Integer)
line eps (s:p:r,v) = (removePublicUserAgents s : mapEndpoint p eps : r, v )

line _ (s:r,v) = (removePublicUserAgents s : r, v )
line _ other = other

inlineQty :: ([String], Integer) -> [String]
inlineQty (xs, x) = xs ++ [show x]

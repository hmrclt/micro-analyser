module MAOutput ( jsonToTable' ) where

import Format (fromBuckets, debucket, removePublicUserAgents)
import qualified Data.Map as M
import Data.Aeson
import Table
import Repo
import MicroProbeOptions
import Data.Maybe (catMaybes)

titles :: Command -> [String]
titles (WhatTalksTo _ ra rb) =
  catMaybes [ Just "Caller"
            , if ra then Just "Endpoint" else Nothing
            , if rb then Just "Response" else Nothing
            , Just "Qty"
            ]
titles _ = []

jsonToTable :: Value -> [String] -> Table String String
jsonToTable jsonV h = table (h++["Qty"]) $ fmap inlineQty b
  where
    b = M.toList (M.fromListWith (+) $ fmap (line []) dataOut)
    dataOut = fromBuckets $ debucket jsonV
    line _ (s:r,v)     = (removePublicUserAgents s : r, v )
    line _ a           = a

jsonToTable' :: Value -> Command -> IO (Table String String)
jsonToTable' jsonV cmd@(WhatTalksTo rName ra _) = do
  endpoints <- repoFromName rName >>= repoEndpoints
  return $ table (titles cmd) $ fmap inlineQty (b endpoints)
  where
    b eps = M.toList (M.fromListWith (+) $ fmap (line eps) dataOut)
    dataOut = fromBuckets $ debucket jsonV
    line :: [Endpoint] -> ([String], Integer) -> ([String], Integer)
    line eps (s:p:r,v) | ra = (removePublicUserAgents s : mapEndpoint p eps : r, v )
    line _ (s:r,v)          = (removePublicUserAgents s : r, v )
    line _ other            = other
jsonToTable' jsonV _        = return $ jsonToTable jsonV []

inlineQty :: ([String], Integer) -> [String]
inlineQty (xs, x) = xs ++ [show x]

module MAOutput ( jsonToTable ) where

import Format (fromBuckets, debucket, stripVars, removePublicUserAgents)
import qualified Data.Map as M
import Data.Aeson
import Table

jsonToTable :: Value -> [String] -> Table String String 
jsonToTable jsonV titles = table (titles++["Qty"]) $ fmap inlineQty b
  where
    b = M.toList (M.fromListWith (+) $ fmap line dataOut)
    dataOut = fromBuckets $ debucket jsonV
    line (s:p:r,v) = (removePublicUserAgents s : stripVars p : r, v )
    line (s:r,v) = (removePublicUserAgents s : r, v )
    line (r,v) = (r, v)
    inlineQty :: ([String], Integer) -> [String]
    inlineQty (xs, x) = xs ++ [show x]

-- output :: Value -> [String] -> Mode -> IO ()
-- output jsonV headers Org = do
--   let dataOut = fromBuckets $ debucket jsonV
--   putStrLn $ "| " ++ intercalate " | " headers ++ " | Qty |"  
--   putStrLn "|-"
--   mapM_ (putStrLn . fmt) $ M.toList (M.fromListWith (+) $ fmap line dataOut)
--   where fmt :: ([String], Integer) -> String
--         fmt (fs, v) = "|" ++ intercalate "|" (fs ++ [show v]) ++ "|"
--         line (s:p:r,v) = (removePublicUserAgents s : stripVars p : r, v )
--         line (s:r,v) = (removePublicUserAgents s : r, v )
--         line (r,v) = (r, v)
-- output jsonV _ DebugResponse = (BL.putStrLn . encode) jsonV

module MAOutput where

import MicroProbeOptions
import Format (fromBuckets, debucket, stripVars)
import qualified Data.Map as M
import Data.List (intercalate)
import Data.Aeson

output :: Value -> Mode -> IO ()
output jsonV Org = do
  let dataOut = fromBuckets $ debucket jsonV
  putStrLn "| Originator | Endpoint | Qty |"
  putStrLn "|-"
  mapM_ (putStrLn . fmt) $ M.toList (M.fromListWith (+) $ fmap (\ ((s:p :_),v) -> (s : stripVars p : [], v ) ) dataOut)
  where
    fmt :: ([String], Integer) -> String
    fmt (fs, v) = "|" ++ (intercalate "|" $ fs ++ [show v]) ++ "|"  
output jsonV DebugResponse = print jsonV

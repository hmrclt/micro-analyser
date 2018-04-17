{-# LANGUAGE OverloadedStrings #-}

module MANetwork where

import Network.Info
import Control.Monad
import System.Exit

isVpnUp :: String -> IO Bool
isVpnUp interface = do
  i <- getNetworkInterfaces
  return $ elem interface $ name <$> i

assertVpn :: Maybe String -> IO ()
assertVpn (Just interface) = do
  up <- isVpnUp interface
  unless up $ die "VPN does not appear to be up"
assertVpn Nothing = return ()

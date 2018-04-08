{-# LANGUAGE OverloadedStrings #-}

module MANetwork where

import Network.Info
import Control.Monad
import System.Exit

isVpnUp :: IO Bool
isVpnUp = do
  i <- getNetworkInterfaces
  return $ elem "tun0" $ name <$> i

assertVpn :: IO ()
assertVpn = do
  up <- isVpnUp
  unless up $ die "VPN does not appear to be up"

{-# LANGUAGE OverloadedStrings #-}

module MAConfigFile (MAConfigFile(..), readConfig) where

import Data.Configurator as C
import System.Exit
import Data.Maybe
import Data.List
import Control.Monad (mfilter)

data MAConfigFile = MAConfigFile { configSecret :: String,
                                   vpnDevice    :: Maybe String
                                 } deriving Show

readConfig :: IO MAConfigFile
readConfig = do
  conf   <- load [ Optional "/etc/microtools/micro-analyser.conf"
                 , Optional "$(HOME)/.microtools/micro-analyser.conf" ]
  secret <- C.lookup conf "secret"
  vpn    <- mfilter (/= "") <$> C.lookup conf "vpncheck"
  case secret of
    Just s  -> return $ MAConfigFile (noBasic s) vpn
    Nothing -> die "Secret is not set - please see /etc/microtools/micro-analyser.conf"

noBasic :: String -> String
noBasic a = fromMaybe a $ stripPrefix "Basic " a

-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Wireless
-- Copyright   :  (c) Jose Antonio Ortega Ruiz
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose Antonio Ortega Ruiz
-- Stability   :  unstable
-- Portability :  unportable
--
-- A monitor reporting ESSID and link quality for wireless interfaces
--
-----------------------------------------------------------------------------

module Plugins.Monitors.Wireless (wirelessConfig, runWireless)  where

import Plugins.Monitors.Common
import IWlib

wirelessConfig :: IO MConfig
wirelessConfig = mkMConfig "<essid> <quality>" ["essid", "quality"]

runWireless :: [String] -> Monitor String
runWireless (iface:_) = do
  wi <- io $ getWirelessInfo iface
  let essid = wiEssid wi
      quality = wiQuality wi
      e = if essid == "" then "N/A" else essid
  q <- if quality >= 0 then showWithColors show quality else return " "
  parseTemplate [e, q]
runWireless _ = return ""
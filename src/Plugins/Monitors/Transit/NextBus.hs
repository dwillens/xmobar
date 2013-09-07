-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.NextBus
-- Copyright   :  (c) Daniel Willenson
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Daniel Willenson <dwillens@hotmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A bus stop monitor for Xmobar
--
-----------------------------------------------------------------------------

module Plugins.Monitors.Transit.NextBus where

import Plugins.Monitors.Common
import Plugins.Monitors.Transit

import Data.Maybe

import Text.XML.Light
import Text.Printf

predict :: TransitStopID -> Element -> TransitPrediction
predict _ doc =
   let
      qNameIs :: String -> QName -> Bool
      qNameIs s = (==) s . qName

      predictions = head $ elChildren doc 

      agency = findAttrBy (qNameIs "agencyTitle") predictions 
      route = findAttrBy (qNameIs "routeTitle") predictions
      stop = findAttrBy (qNameIs "stopTitle") predictions

      preds :: Element -> [Departure]
      preds e = 
         let
            dir = findAttrBy (qNameIs "title") e
            mins = map read . mapMaybe (findAttrBy $ qNameIs "minutes") . 
                     elChildren $ e
         in
            zip (repeat dir) mins

      deps = foldl (++) [] . map preds $ elChildren predictions
   in
      TP agency route stop deps

predictionUrl :: TransitStopID -> String
predictionUrl (a, r, s) = printf 
   ("http://webservices.nextbus.com/service/publicXMLFeed?" ++
      "command=predictions&a=%s&r=%s&s=%s") a r s

runNextBus :: TransitStopID -> [String] -> Monitor String
runNextBus = runTransit predictionUrl parseXMLDoc predict

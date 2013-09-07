-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.MBTA
-- Copyright   :  (c) Daniel Willenson
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Daniel Willenson <dwillens@hotmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A train prediction monitor for Xmobar
--
-----------------------------------------------------------------------------

module Plugins.Monitors.Transit.MBTA where

import Plugins.Monitors.Common
import Plugins.Monitors.Transit

import Data.Maybe
import Text.Printf

import Text.JSON

data Prediction = Prediction {
   predictionStopID     :: String,
   predictionStop       :: String,
   predictionSeconds    :: Integer
}

data Position = Position {
   positionTimestamp    :: Integer,
   positionTrainID      :: String,
   positionCoords       :: (Float, Float),
   positionHeading      :: Int
}

data Trip = Trip {
   tripID            :: String,
   tripDestination   :: String,
   tripPredictions   :: [Prediction]
}

data TripList = TripList {
   tripListCurrentTime     :: Integer,
   tripListLine            :: String,
   tripListTrips           :: [Trip]
}

data Subway = Subway {
   subwayTripList   :: TripList
}


mLookup :: Monad m => String -> [(String, a)] -> m a
mLookup a as = maybe (fail $ "No such element: " ++ a) return $ lookup a as

instance JSON Prediction where
   showJSON p = makeObj
      [ ("StopID", showJSON $ predictionStopID p)
      , ("Stop", showJSON $ predictionStop p)
      , ("Seconds", showJSON $ predictionSeconds p)
      ]
   readJSON (JSObject obj) = do
      let assoc = fromJSObject obj
      sid <- mLookup "StopID" assoc >>= readJSON
      stop <- mLookup "Stop" assoc >>= readJSON
      secs <- mLookup "Seconds" assoc >>= readJSON
      return $ Prediction sid stop secs
   readJSON _ = fail "Couldn't read Prediction"

instance JSON Position where
   showJSON p = makeObj
      [ ("Timestamp", showJSON $ positionTimestamp p)
      , ("Train", showJSON $ positionTrainID p)
      , ("Lat", showJSON $ fst $ positionCoords p)
      , ("Long", showJSON $ snd $ positionCoords p)
      , ("Heading", showJSON $ positionHeading p)
      ]
   readJSON (JSObject obj) = do
      let assoc = fromJSObject obj
      time <- mLookup "Timestamp" assoc >>= readJSON
      trainID <- mLookup "Train" assoc >>= readJSON
      lat <- mLookup "Lat" assoc >>= readJSON
      long <- mLookup "Long" assoc >>= readJSON
      heading <- mLookup "Heading" assoc >>= readJSON
      return $ Position time trainID (lat, long) heading
   readJSON _ = fail "Couldn't read Position"

instance JSON Trip where
   showJSON t = makeObj
      [ ("TripID", showJSON $ tripID t)
      , ("Destination", showJSON $ tripDestination t)
      , ("Predictions", showJSON $ tripPredictions t)
      ]
   readJSON (JSObject obj) = do
      let assoc = fromJSObject obj
      tid <- mLookup "TripID" assoc >>= readJSON
      dest <- mLookup "Destination" assoc >>= readJSON
      preds <- mLookup "Predictions" assoc >>= readJSON
      return $ Trip tid dest preds
   readJSON _ = fail "Couldn't read Trip"

instance JSON TripList where
   showJSON tl = makeObj 
      [ ("CurrentTime", showJSON $ tripListCurrentTime tl)
      , ("Line", showJSON $ tripListLine tl)
      , ("Trips", showJSON $ tripListTrips tl)
      ]
   readJSON (JSObject obj) = do
      let assoc = fromJSObject obj
      currentTime <- mLookup "CurrentTime" assoc >>= readJSON
      line <- mLookup "Line" assoc >>= readJSON
      trips <- mLookup "Trips" assoc >>= readJSON
      return $ TripList currentTime line trips
   readJSON _ = fail "Couldn't read TripList"

instance JSON Subway where
   showJSON l = makeObj [ ("TripList", showJSON $ subwayTripList l) ]
   readJSON (JSObject obj) = do
      let assoc = fromJSObject obj in do
      tripList <- mLookup "TripList" assoc >>= readJSON
      return $ Subway tripList
   readJSON _ = fail "Couldn't read Subway"

parseData :: String -> Maybe Subway
parseData content = do
   case decode content :: Result Subway of
      Ok l -> Just l
      _ -> Nothing

getPrediction :: TransitStopID -> Subway -> TransitPrediction
getPrediction (agencyID, _, stopID) (Subway tl) = 
   let
      route = Just $ tripListLine tl
      fromStop :: Prediction -> Bool
      fromStop = (==) stopID . predictionStopID
      dep :: Trip -> Prediction -> Departure
      dep t p = (Just $ tripDestination t, div (predictionSeconds p) 60)
      departure :: Trip -> Maybe Departure
      departure t = maybe Nothing (Just . dep t) . 
                     listToMaybe . filter fromStop $ tripPredictions t
      deps = mapMaybe departure $ tripListTrips tl
   in 
      TP (Just agencyID) route (Just stopID) deps

predictionUrl :: TransitStopID -> String
predictionUrl (_, r, _) = 
  printf ("http://developer.mbta.com/lib/rthr/%s.json") r

runMBTA :: TransitStopID -> [String] -> Monitor String
runMBTA = runTransit predictionUrl parseData getPrediction

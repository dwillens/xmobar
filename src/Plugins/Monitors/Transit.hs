-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Prediction
-- Copyright   :  (c) Daniel Willenson
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Daniel Willenson <dwillens@hotmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A transit prediction monitor for Xmobar
--
-----------------------------------------------------------------------------

module Plugins.Monitors.Transit
( transitConfig
, TransitPrediction(TP)
, AgencyID
, RouteID
, StopID
, TransitStopID
, Departure
, runTransit
) where

import Plugins.Monitors.Common

import Control.Monad (when)
import Data.List (sortBy, intercalate)
import System.Process
import System.Exit
import System.IO
import System.Console.GetOpt

import Text.Printf

transitConfig :: IO MConfig
transitConfig = mkMConfig 
      "<route>: <departures>"
      [ "agency"
      , "route"
      , "stop"
      , "departures"
      ]

data PredOpts = PredOpts 
   { minTime :: Integer
   , maxPreds :: Int
   }

defaultOpts :: PredOpts
defaultOpts = PredOpts { minTime = 6, maxPreds = 3 }

options :: [OptDescr (PredOpts -> PredOpts)]
options = 
   [ Option "N" ["maxPreds"] (ReqArg (\x o -> o {maxPreds = read x}) "") ""
   , Option "X" ["minTime"] (ReqArg (\x o -> o {minTime = read x}) "") ""
   ]

type AgencyID      = String
type RouteID       = String
type StopID        = String
--type DirectionID   = String
type TransitStopID = (AgencyID, RouteID, StopID)


type AgencyTitle     = Maybe String
type RouteTitle      = Maybe String
type StopTitle       = Maybe String
type DirectionTitle  = Maybe String
type Departure       = (DirectionTitle, Integer)

data TransitPrediction = TP AgencyTitle RouteTitle StopTitle [Departure] 
   deriving (Show)

getData :: String -> IO String
getData url =
   do (i,o,e,p) <- runInteractiveCommand (printf "curl '%s'" url)
      exit <- waitForProcess p
      let closeHandles = do hClose o
                            hClose i
                            hClose e
      case exit of
         ExitSuccess -> do str <- hGetContents o
                           when (str == str) $ return ()
                           closeHandles
                           return str
         _ -> do closeHandles
                 return "Could not retrieve data"

formatPrediction :: TransitPrediction -> PredOpts -> Monitor String
formatPrediction (TP a r s deps) opts =
   let
      earlier :: Departure -> Departure -> Ordering
      earlier x y = compare (snd x) (snd y)       
      preds = take (maxPreds opts) $ 
                   filter (\t -> (>=) t $ minTime opts) $ 
                          map snd $ sortBy earlier deps
      titles = map (maybe "None" id) [a, r, s]
   in do 
      mins <- fmap (intercalate " ") $ mapM (showWithColors show) preds
      parseTemplate $ titles ++ [mins]

predopts :: [String] -> IO PredOpts
predopts argv = 
   case getOpt Permute options argv of
      (o, _, []) -> return $ foldr id defaultOpts o
      (_, _, errs) -> ioError . userError $ concat errs

runTransit :: (TransitStopID -> String) ->  
              (String -> Maybe p) ->
              (TransitStopID -> p -> TransitPrediction) ->
              TransitStopID -> [String] -> Monitor String
runTransit url parse predict stopID args = do
   opts <- io $ predopts args 
   d <- io $ getData $ url stopID
   let prediction = case parse d of
                         Just t -> predict stopID t
                         Nothing -> TP Nothing Nothing Nothing []
   formatPrediction prediction opts

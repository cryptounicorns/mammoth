{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Mammoth.Markets.Tickers.Handlers
  ( getTickerData
  , getChangeData
  )
where

import Control.Lens                             ((&), (.~))
import Control.Monad.IO.Class                   (liftIO)
import Data.Int                                 (Int64)
import Data.Maybe                               (fromMaybe)
import Data.Text                                (pack, toLower, toUpper)
import Data.Time.Clock                          (UTCTime)
import Data.Vector                              (Vector)
import Database.InfluxDB
  ( Field (FieldString)
  , Precision (Millisecond)
  , formatQuery
  , manager
  , precision
  , query
  , queryParams
  , scaleTo
  , (%)
  )
import Database.InfluxDB.Format                 (field, key, string, time)
import Database.InfluxDB.Types                  (Key (Key))
import Mammoth.Markets.Tickers.API              (TickerData (..), TickerPoint (..))
import Mammoth.Markets.Tickers.Changes.Handlers (getChangeData)
import Mammoth.Markets.Tickers.Metrics          (Metric)
import Mammoth.Time
  ( fromMilliseconds
  , getPOSIXMilliseconds
  )
import Network.HTTP.Client                      (Manager)
import Servant                                  (Handler)

getTickerData ∷
    Manager →
    String → String → Metric →
    Maybe Integer → Maybe Integer → Maybe String →
    Handler TickerData
getTickerData mgr marketName currencyPair metricName fromTime toTime resolution = do
  now <- liftIO $ getPOSIXMilliseconds
  let weekAgo = now - 7 * 24 * 60 * 60 * 1000
      tickerPoints = getTickerPoints
        mgr
        marketName
        currencyPair
        metricName
        (fromMilliseconds $ fromMaybe weekAgo fromTime)
        (fromMilliseconds $ fromMaybe now toTime)
        (fromMaybe "1h" resolution)
  points <- liftIO $ fmap fromPoint <$> tickerPoints
  return TickerData{..}

fromPoint ∷ TickerPoint → (Int64, Double)
fromPoint p = (scaleTo Millisecond $ timestamp p, value p)

getTickerPoints ∷
    Manager →
    String → String → Metric →
    UTCTime → UTCTime → String →
    IO (Vector TickerPoint)
getTickerPoints mgr marketName currencyPair metricName fromTime toTime resolution = do
  query qparams $ formatQuery
    ("SELECT MEAN(" % key % ") AS " % key % " FROM " % key % "\
      \ WHERE time > " % time % " AND time < " % time % "\
      \ AND " % key % " = " % field % "\
      \ AND " % key % " = " % field % "\
      \ GROUP BY time(" % string % ")")
    metricField metricLabel measurementName
    fromTime toTime
    marketField (FieldString $ toLower $ pack marketName)
    currencyPairField (FieldString $ toUpper $ pack currencyPair)
    resolution
    where
      qparams = queryParams db
        & manager .~ Right mgr
        & precision .~ Millisecond
      db = "gluttony"
      measurementName = "ticker"
      metricField = Key $ toLower $ pack $ show metricName
      metricLabel = Key "value"
      marketField = "market"
      currencyPairField = "currencyPair"

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Mammoth.Markets.Tickers.Handlers
  ( getTicker
  , getChange
  )
where

import Control.Monad.IO.Class                   (liftIO)
import Data.Int                                 (Int64)
import Data.Maybe                               (fromMaybe)
import Data.Text                                (pack, toLower, toUpper)
import Data.Time.Clock                          (UTCTime)
import Data.Time.Clock.POSIX                    (utcTimeToPOSIXSeconds)
import Data.Vector                              (Vector)
import Database.InfluxDB
  ( Field (FieldString)
  , Precision (Millisecond)
  , QueryParams
  , formatQuery
  , query
  , scaleTo
  , (%)
  )
import Database.InfluxDB.Format                 (field, key, string, time)
import Database.InfluxDB.Types                  (Key (Key))
import Mammoth.Markets.Tickers.API              (Ticker (..), TickerData (..))
import Mammoth.Markets.Tickers.Changes.Handlers (getChange)
import Mammoth.Markets.Tickers.Metrics          (Metric)
import Mammoth.Time                             (getWeekLimitedRange)
import Servant                                  (Handler)

getTicker ∷
  QueryParams →
  String → String → Metric →
  Maybe Integer → Maybe Integer → Maybe String →
  Handler Ticker
getTicker qparams marketName currencyPair metricName fromTime toTime duration = liftIO $ do
  (from, to) <- getWeekLimitedRange (fromTime, toTime)
  points <- fmap fromPoint <$> getData from to resolution
  return Ticker { from = utcTimeToPOSIXSeconds from
                , to   = utcTimeToPOSIXSeconds to
                , ..
                }
  where
    resolution = fromMaybe "1h" duration
    getData    = getTickerData qparams marketName currencyPair metricName

fromPoint ∷ TickerData → (Int64, Double)
fromPoint point = (scaleTo Millisecond $ timestamp point, value point)

getTickerData ∷
  QueryParams →
  String → String → Metric →
  UTCTime → UTCTime → String →
  IO (Vector TickerData)
getTickerData qparams marketName currencyPair metricName fromTime toTime resolution =
  query qparams $ formatQuery
    ("SELECT MEAN(" % key % ") AS " % key % " FROM " % key % "\
      \ WHERE time >= " % time % " AND time <= " % time % "\
      \ AND " % key % " = " % field % "\
      \ AND " % key % " = " % field % "\
      \ GROUP BY time(" % string % ")")
    metricField metricLabel measurementName
    fromTime toTime
    marketField (FieldString $ toLower $ pack marketName)
    currencyPairField (FieldString $ toUpper $ pack currencyPair)
    resolution
    where
      measurementName = "ticker"
      metricField = Key $ toLower $ pack $ show metricName
      metricLabel = Key "value"
      marketField = "market"
      currencyPairField = "currencyPair"

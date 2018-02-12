{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Mammoth.Markets.Tickers.Changes.Handlers (getChange, getChangeData) where

import Control.Monad.IO.Class              (liftIO)
import Data.Text                           (pack, toLower, toUpper)
import Data.Time.Clock                     (UTCTime)
import Data.Time.Clock.POSIX               (utcTimeToPOSIXSeconds)
import Data.Vector                         (Vector, head, null)
import Database.InfluxDB
  ( Field (FieldString)
  , QueryParams
  , formatQuery
  , query
  , (%)
  )
import Database.InfluxDB.Format            (field, key, time)
import Database.InfluxDB.Types             (Key (Key))
import Mammoth.Markets.Tickers.Changes.API (Change (..), ChangeData (..))
import Mammoth.Markets.Tickers.Metrics     (Metric)
import Mammoth.Time                        (getWeekLimitedRange)
import Servant                             (Handler)

getChange ∷
  QueryParams →
  String → String → Metric →
  Maybe Integer → Maybe Integer →
  Handler Change
getChange qparams marketName currencyPair metricName fromTime toTime = liftIO $ do
  (from, to) <- getWeekLimitedRange (fromTime, toTime)
  ChangeData {value = change} <- getData from to
  return Change { from = utcTimeToPOSIXSeconds from
                , to   = utcTimeToPOSIXSeconds to
                , ..
                }
  where
    getData = getChangeData qparams marketName currencyPair metricName

getChangeData ∷
  QueryParams →
  String → String → Metric →
  UTCTime → UTCTime →
  IO ChangeData
getChangeData params marketName currencyPair metricName fromTime toTime =
  unwrapData <$> query params sql
  where
    sql = formatQuery
      ("SELECT \
       \ 100 - (first(" % key % ") / last(" % key % ")) * 100 AS " % key % "\
       \ FROM " % key % "\
       \ WHERE time >= " % time % " AND time <= " % time % "\
       \ AND " % key % " = " % field % "\
       \ AND " % key % " = " % field)
      metricField metricField metricLabel
      measurementName
      fromTime toTime
      marketField (FieldString $ toLower $ pack marketName)
      currencyPairField (FieldString $ toUpper $ pack currencyPair)
    measurementName = "ticker"
    metricField = Key $ toLower $ pack $ show metricName
    metricLabel = Key "value"
    marketField = "market"
    currencyPairField = "currencyPair"

unwrapData ∷ Vector ChangeData → ChangeData
unwrapData vector
  | Data.Vector.null vector = ChangeData 0
  | otherwise               = Data.Vector.head vector

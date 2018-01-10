{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Mammoth.Markets.Tickers.Changes.Handlers (getChangeData, getChange) where

import Control.Lens                        ((&), (.~))
import Control.Monad.IO.Class              (liftIO)
import Data.Maybe                          (fromMaybe)
import Data.Text                           (pack, toLower, toUpper)
import Data.Time.Clock                     (UTCTime)
import Data.Vector                         (Vector, head)
import Database.InfluxDB
  ( Field (FieldString)
  , Precision (Millisecond)
  , formatQuery
  , manager
  , precision
  , query
  , queryParams
  , (%)
  )
import Database.InfluxDB.Format            (field, key, time)
import Database.InfluxDB.Types             (Key (Key))
import Mammoth.Markets.Tickers.Changes.API (ChangeData (..))
import Mammoth.Markets.Tickers.Metrics     (Metric)
import Mammoth.Time                        (fromMilliseconds, getPOSIXMilliseconds)
import Network.HTTP.Client                 (Manager)
import Servant                             (Handler)

getChangeData ∷
    Manager →
    String → String → Metric →
    Maybe Integer → Maybe Integer →
    Handler ChangeData
getChangeData mgr marketName currencyPair metricName fromTime toTime = do
  now <- liftIO getPOSIXMilliseconds
  let weekAgo = now - 7 * 24 * 60 * 60 * 1000
  change <- liftIO $ getChange
        mgr
        marketName
        currencyPair
        metricName
        (fromMilliseconds $ fromMaybe weekAgo fromTime)
        (fromMilliseconds $ fromMaybe now toTime)
  return $ Data.Vector.head change

getChange ∷
    Manager →
    String → String → Metric →
    UTCTime → UTCTime →
    IO (Vector ChangeData)
getChange mgr marketName currencyPair metricName fromTime toTime = do
  query qparams $ formatQuery
    ("SELECT \
      \ 100 - (first(" % key % ") / last(" % key % ")) * 100 AS " % key % "\
      \ FROM " % key % "\
      \ WHERE time > " % time % " AND time < " % time % "\
      \ AND " % key % " = " % field % "\
      \ AND " % key % " = " % field)
    metricField metricField metricLabel
    measurementName
    fromTime toTime
    marketField (FieldString $ toLower $ pack marketName)
    currencyPairField (FieldString $ toUpper $ pack currencyPair)
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

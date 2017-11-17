{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Mammoth.Server (server) where

import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (pack, toLower)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Vector (Vector)
import Database.InfluxDB (
    formatQuery,
    manager,
    scaleTo,
    query,
    queryParams,
    precision,
    (%),
    Field(FieldString),
    Precision(Millisecond))
import Database.InfluxDB.Types (Key(Key))
import Database.InfluxDB.Format (field, key, string, time)
import Mammoth.API (Api, TickerData(..), TickerHistoryPoint(..), TickerMetric)
import Network.HTTP.Client (Manager)
import Servant (Handler, Server)

server :: Manager -> Server Api
server = getTickerData

getTickerData :: (
    Manager ->
    String -> String -> TickerMetric ->
    Maybe Integer -> Maybe Integer -> Maybe String ->
    Handler TickerData)
getTickerData
    mgr marketId currencyPair metricId
    fromTime untilTime resolution = do
  let historyPoints = getTickerHistory mgr marketId currencyPair metricId (fromMilliseconds fromTime) (fromMilliseconds untilTime) (fromMaybe "1m" resolution)
  history <- liftIO $ fmap fromHistoryPoint <$> historyPoints
  return $ TickerData{..}

fromHistoryPoint :: TickerHistoryPoint -> (Int64, Double)
fromHistoryPoint p = (scaleTo Millisecond $ timestamp p, price p)

fromMilliseconds :: Maybe Integer -> UTCTime
fromMilliseconds = posixSecondsToUTCTime . (/ 1000) . fromInteger . fromJust

getTickerHistory :: (
    Manager ->
    String -> String -> TickerMetric ->
    UTCTime -> UTCTime -> String ->
    IO (Vector TickerHistoryPoint))
getTickerHistory
    mgr marketId pair metricId
    fromTime untilTime resolution = do
  query qparams $ formatQuery
    ("SELECT MEAN(" % key % ") AS " % key % " FROM " % key % "\
      \ WHERE \"time\" > "% time % " AND \"time\" < "% time % "\
      \ AND " % key % " = " % field % "\
      \ AND " % key % " = " % field % "\
      \ GROUP BY time(" % string % ")")
    metricField metricField measurementName
    fromTime untilTime
    marketField (FieldString $ pack marketId)
    pairField (FieldString $ pack pair)
    resolution
    where
      qparams = queryParams db
        & manager .~ Right mgr
        & precision .~ Millisecond
      db = "cryptounicorns"
      measurementName = "ticker"
      metricField = Key $ toLower $ pack $ show metricId
      marketField = "market"
      pairField = "currency-pair"
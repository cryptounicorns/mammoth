{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Mammoth.API (
    Api,
    api,
    TickerData(..),
    TickerHistory,
    TickerHistoryPoint(..),
    TickerMetric
) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Int (Int64)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Vector (Vector)
import Database.InfluxDB (
    Field(FieldFloat),
    QueryResults(parseResults),
    getField,
    parsePOSIXTime,
    parseResultsWith,
    parseQueryField)
import GHC.Generics (Generic)
import Servant ((:>), Capture, Get, JSON, Proxy(Proxy), QueryParam)
import Web.HttpApiData (FromHttpApiData(parseUrlPiece), parseBoundedTextData)

type Api = "v1" :> ApiV1

type ApiV1 =
  "markets" :> MarketApi

type MarketApi =
  Capture "marketId" String
  :> "tickers"
  :> Capture "currencyPair" String
  :> Capture "metric" TickerMetric
  :> QueryParam "from" Integer
  :> QueryParam "until" Integer
  :> QueryParam "resolution" String
  :> Get '[JSON] TickerData

api :: Proxy Api
api = Proxy

data TickerMetric = High | Low | Vol | Last | Buy | Sell
    deriving (Eq, Show, Generic, Enum, Bounded)

instance FromHttpApiData TickerMetric where
    parseUrlPiece = parseBoundedTextData

data TickerData = TickerData {
    marketId :: String,
    currencyPair :: String,
    history :: TickerHistory
} deriving (Eq, Show, Generic)

instance ToJSON TickerData
instance FromJSON TickerData

type TickerHistory = Vector (Int64, Double)

data TickerHistoryPoint = TickerHistoryPoint {
    timestamp :: POSIXTime,
    price :: Double
} deriving (Eq, Show, Generic)

instance ToJSON TickerHistoryPoint
instance FromJSON TickerHistoryPoint

instance QueryResults TickerHistoryPoint where
    parseResults prec' = parseResultsWith $ \_ _ columns fields -> do
      timestamp <- getField "time" columns fields >>= parsePOSIXTime prec'
      FieldFloat price <- getField "last" columns fields >>= parseQueryField
      return TickerHistoryPoint{..}
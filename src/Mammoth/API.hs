{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Mammoth.API
  ( Api
  , api
  , TickerData(..)
  , TickerPoints
  , TickerPoint(..)
  , TickerMetric
  )
where

import           Data.Aeson            (FromJSON, ToJSON)
import           Data.Int              (Int64)
import           Data.Time.Clock.POSIX (POSIXTime)
import           Data.Vector           (Vector)
import           Database.InfluxDB
  ( Field (FieldFloat)
  , QueryResults (parseResults)
  , getField
  , parsePOSIXTime
  , parseQueryField
  , parseResultsWith
  )
import           GHC.Generics          (Generic)
import           Servant               ((:>), Capture, Get, JSON, Proxy (Proxy), QueryParam)
import           Web.HttpApiData       (FromHttpApiData (parseUrlPiece), parseBoundedTextData)

type Api = "api"  :> ApiV1
type ApiV1 = "v1" :> "markets" :> MarketApi

type MarketApi =
  Capture "marketName" String
  :> "tickers"
  :> Capture "currencyPair" String
  :> Capture "metric" TickerMetric
  :> QueryParam "from" Integer
  :> QueryParam "to" Integer
  :> QueryParam "resolution" String
  :> Get '[JSON] TickerData

api ∷ Proxy Api
api = Proxy

data TickerMetric = High | Low | Vol | Last | Buy | Sell
    deriving (Eq, Show, Generic, Enum, Bounded)

instance FromHttpApiData TickerMetric where
    parseUrlPiece = parseBoundedTextData

data TickerData = TickerData {
    marketName   :: String,
    currencyPair :: String,
    points       :: TickerPoints
} deriving (Eq, Show, Generic)

instance ToJSON TickerData
instance FromJSON TickerData

type TickerPoints = Vector (Int64, Double)

data TickerPoint = TickerPoint {
    timestamp :: POSIXTime,
    value     :: Double
} deriving (Eq, Show, Generic)

instance ToJSON TickerPoint
instance FromJSON TickerPoint

instance QueryResults TickerPoint where
    parseResults prec' = parseResultsWith $ \_ _ columns fields -> do
      timestamp <- getField "time" columns fields >>= parsePOSIXTime prec'
      FieldFloat value <- getField "value" columns fields >>= parseQueryField
      return TickerPoint{..}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Mammoth.Markets.Tickers.API
  ( TickersApi
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
import           Servant               ((:>), Capture, Get, JSON, QueryParam)
import           Web.HttpApiData       (FromHttpApiData (parseUrlPiece), parseBoundedTextData)

type TickersApi = "tickers"
  :> Capture "currencyPair" String
  :> Capture "metric" TickerMetric
  :> QueryParam "from" Integer
  :> QueryParam "to" Integer
  :> QueryParam "resolution" String
  :> Get '[JSON] TickerData

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

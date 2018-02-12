{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Mammoth.Markets.Tickers.API
  ( TickersApi
  , Ticker(..)
  , TickerData(..)
  , TickerPoints
  )
where

import Control.Lens                        (mapped, (&), (?~))
import Data.Aeson                          (FromJSON, ToJSON, toJSON)
import Data.Int                            (Int64)
import Data.Swagger
  ( ToSchema
  , declareNamedSchema
  , defaultSchemaOptions
  , description
  , example
  , genericDeclareNamedSchema
  , schema
  )
import Data.Time.Clock.POSIX               (POSIXTime)
import Data.Vector                         (Vector, generate)
import Database.InfluxDB
  ( Field (FieldFloat)
  , QueryResults (parseResults)
  , getField
  , parsePOSIXTime
  , parseQueryField
  , parseResultsWith
  )
import GHC.Generics                        (Generic)
import Mammoth.Markets.Tickers.Changes.API (ChangesApi)
import Mammoth.Markets.Tickers.Metrics     (Metric (Last))
import Servant
  ( (:<|>)
  , (:>)
  , Capture
  , Get
  , JSON
  , QueryParam
  )

type TickersApi
   = Capture "marketName" String
   :> "tickers"
   :> Capture    "currencyPair" String
   :> Capture    "metric"       Metric
   :> QueryParam "from"         Integer
   :> QueryParam "to"           Integer
   :> QueryParam "resolution"   String
   :> Get        '[JSON]        Ticker
   :<|> ChangesApi

data Ticker = Ticker
  { marketName   :: String
  , currencyPair :: String
  , metricName   :: Metric
  , from         :: POSIXTime
  , to           :: POSIXTime
  , resolution   :: String
  , points       :: TickerPoints
  } deriving (Eq, Show, Generic)

instance ToJSON Ticker
instance FromJSON Ticker

instance ToSchema Ticker where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "Ticker data"
    & mapped.schema.example ?~ toJSON (Ticker
                                       "bitfinex"
                                       "BTC-USD"
                                       Last
                                       (1513901024 * 1000)
                                       (1513769299 * 1000)
                                       "1h"
                                       (generate 5 mkTickerPoint))
    where
      mkTickerPoint i = ((1513901024 + fromIntegral i) * 1000, 17635.2 + fromIntegral i)

type TickerPoints = Vector (Int64, Double)

data TickerData = TickerData
  { timestamp :: POSIXTime
  , value     :: Double
  } deriving (Eq, Show, Generic)

instance ToJSON TickerData
instance FromJSON TickerData

instance QueryResults TickerData where
    parseResults precision = parseResultsWith $ \_ _ columns fields → do
      timestamp <- getField "time" columns fields >>= parsePOSIXTime precision
      FieldFloat value <- getField "value" columns fields >>= parseQueryField
      return TickerData{..}

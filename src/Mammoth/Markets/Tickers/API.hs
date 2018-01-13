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
import Mammoth.Markets.Tickers.Metrics     (Metric)
import Servant
  ( (:<|>)
  , (:>)
  , Capture
  , Get
  , JSON
  , QueryParam
  )

type TickersApi
   =  "markets"
   :> Capture "marketName" String
   :> "tickers"
   :> Capture    "currencyPair" String
   :> Capture    "metric"       Metric
   :> QueryParam "from"         Integer
   :> QueryParam "to"           Integer
   :> QueryParam "resolution"   String
   :> Get        '[JSON]        TickerData
   :<|> ChangesApi

data TickerData = TickerData
  { marketName   :: String
  , currencyPair :: String
  , from         :: POSIXTime
  , to           :: POSIXTime
  , points       :: TickerPoints
  } deriving (Eq, Show, Generic)

instance ToJSON TickerData
instance FromJSON TickerData
instance ToSchema TickerData where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "Ticker data"
    & mapped.schema.example ?~ toJSON (TickerData
                                       "bitfinex"
                                       "BTC-USD"
                                       (1513901024 * 1000)
                                       (1513769299 * 1000)
                                       (generate 5 mkTickerPoint))
    where
      mkTickerPoint i = ((1513901024 + fromIntegral i) * 1000, 17635.2 + fromIntegral i)

type TickerPoints = Vector (Int64, Double)

data TickerPoint = TickerPoint
  { timestamp :: POSIXTime
  , value     :: Double
  } deriving (Eq, Show, Generic)

instance ToJSON TickerPoint
instance FromJSON TickerPoint

instance QueryResults TickerPoint where
    parseResults precision = parseResultsWith $ \_ _ columns fields -> do
      timestamp <- getField "time" columns fields >>= parsePOSIXTime precision
      FieldFloat value <- getField "value" columns fields >>= parseQueryField
      return TickerPoint{..}

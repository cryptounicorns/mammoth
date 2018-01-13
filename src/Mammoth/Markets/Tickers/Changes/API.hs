{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Mammoth.Markets.Tickers.Changes.API
  ( ChangesApi
  , Change(..)
  , ChangeData(..)
  )
where

import Control.Lens                    (mapped, (&), (?~))
import Data.Aeson                      (FromJSON, ToJSON, toJSON)
import Data.Swagger
  ( ToSchema
  , declareNamedSchema
  , defaultSchemaOptions
  , description
  , example
  , genericDeclareNamedSchema
  , schema
  )
import Data.Time.Clock.POSIX           (POSIXTime)
import Database.InfluxDB
  ( Field (FieldFloat)
  , QueryResults (parseResults)
  , getField
  , parseQueryField
  , parseResultsWith
  )
import GHC.Generics                    (Generic)
import Mammoth.Markets.Tickers.Metrics (Metric (Last))
import Servant                         ((:>), Capture, Get, JSON, QueryParam)

type ChangesApi
  = "markets"
  :> Capture "marketName" String
  :> "tickers"
  :> "changes"
  :> Capture "currencyPair" String
  :> Capture "metric" Metric
  :> QueryParam "from" Integer
  :> QueryParam "to" Integer
  :> Get '[JSON] Change

data Change = Change
  { marketName   :: String
  , currencyPair :: String
  , metricName   :: Metric
  , from         :: POSIXTime
  , to           :: POSIXTime
  , change       :: Double
  } deriving (Eq, Show, Generic)

instance ToJSON Change
instance FromJSON Change
instance ToSchema Change where
    declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.description ?~ "Ticker data percent change for a period of time"
        & mapped.schema.example ?~ toJSON (Change
                                           "bitfinex"
                                           "BTC-USD"
                                           Last
                                           1513769217384
                                           1513769299999
                                           95)

data ChangeData = ChangeData
  { value :: Double } deriving (Eq, Show, Generic)

instance ToJSON ChangeData
instance FromJSON ChangeData

instance QueryResults ChangeData where
    parseResults _ = parseResultsWith
      $ \_ _ columns fields -> do getField "value" columns fields
        >>= parseQueryField
        >>= \(FieldFloat value) -> do return ChangeData{..}

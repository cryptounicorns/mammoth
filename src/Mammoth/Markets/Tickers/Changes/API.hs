{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Mammoth.Markets.Tickers.Changes.API
  ( ChangesApi
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
  , parsePOSIXTime
  , parseQueryField
  , parseResultsWith
  )
import GHC.Generics                    (Generic)
import Mammoth.Markets.Tickers.Metrics (Metric)
import Servant                         ((:>), Capture, Get, JSON, QueryParam)

type ChangesApi
  =  "markets"
  :> Capture "marketName" String
  :> "tickers"
  :> "changes"
  :> Capture "currencyPair" String
  :> Capture "metric" Metric
  :> QueryParam "from" Integer
  :> QueryParam "to" Integer
  :> Get '[JSON] ChangeData

data ChangeData = ChangeData {
  timestamp :: POSIXTime,
  value     :: Double
} deriving (Eq, Show, Generic)

instance ToJSON ChangeData
instance FromJSON ChangeData
instance ToSchema ChangeData where
    declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.description ?~ "Ticker data percent change for a period of time"
        & mapped.schema.example ?~ toJSON (ChangeData 1513901024000 95)

instance QueryResults ChangeData where
    parseResults prec' = parseResultsWith $ \_ _ columns fields -> do
      timestamp <- getField "time" columns fields >>= parsePOSIXTime prec'
      FieldFloat value <- getField "value" columns fields >>= parseQueryField
      return ChangeData{..}

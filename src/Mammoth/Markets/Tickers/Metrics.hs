{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Mammoth.Markets.Tickers.Metrics (Metric(..)) where

import Control.Lens    (mapped, (&), (?~))
import Data.Aeson      (FromJSON, ToJSON, toJSON)
import Data.Swagger    (ToParamSchema)
import Data.Swagger
  ( ToSchema
  , declareNamedSchema
  , defaultSchemaOptions
  , description
  , example
  , genericDeclareNamedSchema
  , schema
  )
import GHC.Generics    (Generic)
import Web.HttpApiData (FromHttpApiData (parseUrlPiece), parseBoundedTextData)

data Metric = High | Low | Vol | Last | Buy | Sell
    deriving (Eq, Read, Show, Generic, Enum, Bounded)

instance ToJSON Metric
instance FromJSON Metric
instance ToSchema Metric where
    declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.description ?~ "Ticker metric"
        & mapped.schema.example ?~ toJSON Last

instance FromHttpApiData Metric where
    parseUrlPiece = parseBoundedTextData

instance ToParamSchema Metric

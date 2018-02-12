{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE UnicodeSyntax      #-}

module Mammoth.Markets.Tickers.Metrics (Metric(..)) where

import Control.Lens    (mapped, (&), (?~))
import Data.Aeson      (FromJSON(parseJSON), ToJSON (toJSON), Value (String), toJSON)
import Data.Data       (Data, showConstr, toConstr)
import Data.Strings    (strToLower, strCapitalize)
import Data.Swagger
  ( ToParamSchema
  , ToSchema
  , declareNamedSchema
  , defaultSchemaOptions
  , description
  , example
  , genericDeclareNamedSchema
  , schema
  )
import Data.Text       (pack, unpack)
import Data.Typeable   (Typeable)
import GHC.Generics    (Generic)
import Web.HttpApiData (FromHttpApiData (parseUrlPiece), parseBoundedTextData)

data Metric = High | Low | Vol | Last | Buy | Sell
    deriving (Eq, Typeable, Data, Generic, Enum, Bounded)

instance Show Metric where
  show = strToLower . showConstr . toConstr

instance Read Metric where
  readsPrec prec = map f . readsPrec prec
    where
      f (k, v) = (toEnum (k - 1), strCapitalize v)

instance ToJSON Metric where
  toJSON = String . pack . show

instance FromJSON Metric where
  parseJSON (String v) = return $ read . unpack $ v
  parseJSON _          = fail "Failed to parse Metric!"

instance ToSchema Metric where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "Ticker metric"
    & mapped.schema.example ?~ toJSON Last

instance FromHttpApiData Metric where
    parseUrlPiece = parseBoundedTextData

instance ToParamSchema Metric

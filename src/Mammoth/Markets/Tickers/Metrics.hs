{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Mammoth.Markets.Tickers.Metrics (Metric) where

import Data.Swagger    (ToParamSchema)
import GHC.Generics    (Generic)
import Web.HttpApiData (FromHttpApiData (parseUrlPiece), parseBoundedTextData)

data Metric = High | Low | Vol | Last | Buy | Sell
    deriving (Eq, Read, Show, Generic, Enum, Bounded)

instance FromHttpApiData Metric where
    parseUrlPiece = parseBoundedTextData

instance ToParamSchema Metric

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Mammoth.Markets.Server (marketsServer) where

import Database.InfluxDB              (QueryParams)
import Mammoth.Markets.API            (MarketsApi)
import Mammoth.Markets.Tickers.Server (tickersServer)
import Servant                        (Server)

marketsServer ∷ QueryParams → Server MarketsApi
marketsServer = tickersServer

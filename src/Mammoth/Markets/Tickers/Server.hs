{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Mammoth.Markets.Tickers.Server (tickersServer) where

import Database.InfluxDB                (QueryParams)
import Mammoth.Markets.Tickers.API      (TickersApi)
import Mammoth.Markets.Tickers.Handlers (getChange, getTicker)
import Servant                          (Server)
import Servant.API                      ((:<|>) ((:<|>)))

tickersServer ∷ QueryParams → Server TickersApi
tickersServer p = getTicker p :<|> getChange p

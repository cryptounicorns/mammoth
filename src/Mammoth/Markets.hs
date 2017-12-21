{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Mammoth.Markets (MarketsApi) where

import           Mammoth.Markets.Tickers.API (TickersApi)
import           Servant                     ((:>), Capture)

type MarketsApi = "markets" :> MarketApi

type MarketApi =
  Capture "marketName" String
  :> TickersApi

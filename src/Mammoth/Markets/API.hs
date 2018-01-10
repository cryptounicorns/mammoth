{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Mammoth.Markets.API (MarketsApi) where

import Mammoth.Markets.Tickers.API (TickersApi)

type MarketsApi = TickersApi

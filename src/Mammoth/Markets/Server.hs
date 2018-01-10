{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Mammoth.Markets.Server (marketsServer) where

import Mammoth.Markets.API            (MarketsApi)
import Mammoth.Markets.Tickers.Server (tickersServer)
import Network.HTTP.Client            (Manager)
import Servant                        (Server)

marketsServer ∷ Manager → Server MarketsApi
marketsServer = tickersServer

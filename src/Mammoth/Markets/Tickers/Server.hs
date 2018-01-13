{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Mammoth.Markets.Tickers.Server (tickersServer) where

import Mammoth.Markets.Tickers.API      (TickersApi)
import Mammoth.Markets.Tickers.Handlers (getChange, getTickerData)
import Network.HTTP.Client              (Manager)
import Servant                          (Server)
import Servant.API                      ((:<|>) ((:<|>)))

tickersServer ∷ Manager → Server TickersApi
tickersServer mgr = getTickerData mgr :<|> getChange mgr

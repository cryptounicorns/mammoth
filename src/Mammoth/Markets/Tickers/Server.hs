{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Mammoth.Markets.Tickers.Server (tickersServer) where

import Mammoth.Markets.Tickers.API      (TickersApi)
import Mammoth.Markets.Tickers.Handlers (getChange, getTicker)
import Network.HTTP.Client              (Manager)
import Servant                          (Server)
import Servant.API                      ((:<|>) ((:<|>)))

tickersServer ∷ Manager → Server TickersApi
tickersServer mgr = getTicker mgr :<|> getChange mgr

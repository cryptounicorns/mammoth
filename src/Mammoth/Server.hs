{-# LANGUAGE UnicodeSyntax #-}

module Mammoth.Server (server) where

import           Mammoth.API                      (Api)
import           Mammoth.Markets.Tickers.Handlers (getTickerData)
import           Network.HTTP.Client              (Manager)
import           Servant                          (Server)

server ∷ Manager → Server Api
server = getTickerData -- use <|> combinator to "compose" the handlers

{-# LANGUAGE UnicodeSyntax #-}

module Mammoth.Server (server) where

import           Mammoth.API                      (ApiWithDocs, swaggerDoc)
import           Mammoth.Markets.Tickers.Handlers (getTickerData)
import           Network.HTTP.Client              (Manager)
import           Servant
import           Servant.Swagger.UI               (swaggerSchemaUIServer)

server ∷ Manager → Server ApiWithDocs
server mgr = getTickerData mgr :<|> swaggerSchemaUIServer swaggerDoc

{-# LANGUAGE UnicodeSyntax #-}

module Mammoth.Server (server) where

import qualified Database.InfluxDB
import           Mammoth.API            (ApiWithDocs, swaggerDoc)
import           Mammoth.Markets.Server (marketsServer)
import           Servant
import           Servant.Swagger.UI     (swaggerSchemaUIServer)

server ∷ Database.InfluxDB.QueryParams → Server ApiWithDocs
server p = marketsServer p :<|> swaggerSchemaUIServer swaggerDoc

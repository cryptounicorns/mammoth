{-# LANGUAGE UnicodeSyntax #-}

module Mammoth.Server (server) where

import Mammoth.API                      (ApiWithDocs, swaggerDoc)
import Mammoth.Markets.Server           (marketsServer)
import Network.HTTP.Client              (Manager)
import Servant                          (Server)
import Servant.API                      ((:<|>) ((:<|>)))
import Servant.Swagger.UI               (swaggerSchemaUIServer)

server ∷ Manager → Server ApiWithDocs
server mgr = marketsServer mgr :<|> swaggerSchemaUIServer swaggerDoc

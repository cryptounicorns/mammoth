{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Mammoth.API
  ( Api
  , ApiWithDocs
  , api
  , apiWithDocs
  , swaggerDoc
  )
where

import Control.Lens        ((&), (.~), (?~))
import Data.Swagger        (Swagger, description, info, title, version)
import Mammoth.Markets.API (MarketsApi)
import Servant             ((:<|>), (:>), Capture, Proxy (Proxy))
import Servant.Swagger     (toSwagger)
import Servant.Swagger.UI  (SwaggerSchemaUI)

type Api = "api" :> "v1" :> MarketsApi

type ApiWithDocs = Api :<|> SwaggerSchemaUI "doc" "swagger.json"

api ∷ Proxy Api
api = Proxy

apiWithDocs ∷ Proxy ApiWithDocs
apiWithDocs = Proxy

-- FIXME: version should be determined at build time
-- FIXME: title and description should be configurable
swaggerDoc ∷ Swagger
swaggerDoc = toSwagger (Proxy :: Proxy Api)
  & info.title .~ "Cryptounicorns API"
  & info.version .~ "2017.12"
  & info.description ?~ "This is an API for cryptocurrency markets"

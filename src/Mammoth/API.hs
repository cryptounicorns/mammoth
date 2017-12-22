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

import           Control.Lens       ((&), (.~), (?~))
import           Data.Swagger       (Swagger, description, info, title, version)
import           Mammoth.Markets    (MarketsApi)
import           Servant            ((:<|>), (:>), Proxy (Proxy))
import           Servant.Swagger    (toSwagger)
import           Servant.Swagger.UI (SwaggerSchemaUI)

type Api = "api" :> ApiV1
type ApiV1 = "v1" :> MarketsApi

type ApiWithDocs = Api :<|> SwaggerSchemaUI "doc" "swagger.json"

api ∷ Proxy Api
api = Proxy

apiWithDocs ∷ Proxy ApiWithDocs
apiWithDocs = Proxy

swaggerDoc ∷ Swagger
swaggerDoc = toSwagger (Proxy :: Proxy Api)
  & info.title .~ "Cryptounicorns API"
  & info.version .~ "2017.12"
  & info.description ?~ "This is an API for cryptocurrency markets"

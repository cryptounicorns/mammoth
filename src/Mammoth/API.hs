{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Mammoth.API
  ( Api
  , api
  )
where

import           Mammoth.Markets (MarketsApi)
import           Servant         ((:>), Capture, Proxy (Proxy))

type Api = "api" :> ApiV1
type ApiV1 = "v1" :> MarketsApi

api ∷ Proxy Api
api = Proxy

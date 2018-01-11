{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE UnicodeSyntax     #-}

module App where

import           Control.Lens                         ((&), (.~))
import           Data.Maybe                           (fromMaybe)
import           Data.Text                            (pack)
import           Database.InfluxDB
  ( Precision (Millisecond)
  , QueryParams
  , host
  , manager
  , port
  , precision
  , queryParams
  , server
  )
import           GHC.Generics                         (Generic)
import           Mammoth.API                          (apiWithDocs)
import qualified Mammoth.Server
import           Network.HTTP.Client                  (Manager, defaultManagerSettings, newManager)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant                              (serve)
import           System.Envy                          (DefConfig, FromEnv, decode, defConfig)
import           System.IO

data Config = Config {
  mammothListenPort :: Int,
  influxDbHost      :: String,
  influxDbPort      :: Int
} deriving (Generic, Show)

instance DefConfig Config where
  defConfig = Config {
    mammothListenPort = 3000,
    influxDbHost = "localhost",
    influxDbPort = 8086
  }

instance FromEnv Config

run ∷ IO ()
run = do
  cfg <- getConfig
  settings <- getSettings cfg
  runSettings settings =<< mkApp cfg

getConfig ∷ IO Config
getConfig = do
  c <- decode
  return $ fromMaybe defConfig c

getSettings ∷ Config → IO Settings
getSettings cfg = return $ printPort $ setPort' cfg defaultSettings
  where
    printPort s = setBeforeMainLoop (hPutStrLn stderr $ "listening on port " ++ show (getPort s)) s
    setPort' = setPort . mammothListenPort

getInfluxDbParams ∷ Manager → Config → QueryParams
getInfluxDbParams mgr cfg = queryParams "gluttony"
        & manager .~ Right mgr
        & precision .~ Millisecond
        & server.host .~ pack (influxDbHost cfg)
        & server.port .~ influxDbPort cfg

mkApp ∷ Config → IO Application
mkApp cfg = do
  mgr <- newManager defaultManagerSettings
  return $ logStdoutDev $ serve apiWithDocs $ Mammoth.Server.server $ getInfluxDbParams mgr cfg

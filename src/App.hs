{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module App where

import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant (serve)
import System.IO
import Mammoth.API (api)
import Mammoth.Server (server)

run :: IO ()
run = do
  let port' = 3000
      settings =
        setPort port' $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port')) $
        defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = do
  mgr <- newManager defaultManagerSettings
  return $ logStdoutDev $ serve api $ server mgr
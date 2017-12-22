{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

module App where

import           Mammoth.API                          (apiWithDocs)
import           Mammoth.Server                       (server)
import           Network.HTTP.Client                  (defaultManagerSettings, newManager)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant                              (serve)
import           System.IO

run ∷ IO ()
run = do
  let port' = 3000
      settings =
        setPort port' $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port'))
        defaultSettings
  runSettings settings =<< mkApp

mkApp ∷ IO Application
mkApp = do
  mgr <- newManager defaultManagerSettings
  return $ logStdoutDev $ serve apiWithDocs $ server mgr

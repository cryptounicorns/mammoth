{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module App where

import           Control.Lens
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson
import           Data.Int (Int64)
import           Data.Text (pack)
import           Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import qualified Data.Vector as V
import           GHC.Generics
import qualified Network.HTTP.Client as HC
import           Network.Wai
import           Network.Wai.Handler.Warp
import qualified Network.Wai.Middleware.RequestLogger as RL
import           Servant
import           System.IO
import           Database.InfluxDB
import qualified Database.InfluxDB.Format as F

type Api = "v1" :> ApiV1

type ApiV1 =
  "markets" :> MarketApi

type MarketApi =
  Capture "marketId" String
  :> "tickers"
  :> Capture "pair" String
  :> QueryParam "from" Integer
  :> QueryParam "resolution" String
  :> Get '[JSON] TickerData

api :: Proxy Api
api = Proxy
    
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
  mgr <- HC.newManager HC.defaultManagerSettings
  return $ RL.logStdoutDev $ serve api $ apiServer mgr

apiServer :: HC.Manager -> Servant.Server Api
apiServer = getTickerData

getTickerData :: HC.Manager -> String -> String -> Maybe Integer -> Maybe String -> Handler TickerData
getTickerData _ _ _ Nothing _ = throwError err400
getTickerData mgr marketId pair fromTime Nothing = getTickerData mgr marketId pair fromTime (Just "1m")
getTickerData mgr marketId pair (Just fromTime) (Just resolution) = do
  history <- liftIO $ getTickerHistory mgr (fromMilliseconds fromTime) resolution marketId pair
  return $ TickerData marketId pair $ fmap fromHistoryPoint history

fromHistoryPoint :: TickerHistoryPoint -> (Int64, Double)
fromHistoryPoint p = (scaleTo Millisecond $ time p, price p)

fromMilliseconds :: Integer -> POSIXTime
fromMilliseconds = (/ 1000) . fromInteger

getTickerHistory :: HC.Manager -> POSIXTime -> String -> String -> String -> IO (V.Vector TickerHistoryPoint)
getTickerHistory mgr fromTime resolution marketId pair = do
  query qparams $ formatQuery
    ("SELECT MEAN(" % F.key % ") AS " % F.key % " FROM " % F.key % "\
      \ WHERE \"time\" > "% F.time % "\
      \ AND " % F.key % " = " % F.field % "\
      \ AND " % F.key % " = " % F.field % "\
      \ GROUP BY time(" % F.string % ")")
    field field measurementName
    (posixSecondsToUTCTime fromTime)
    marketField (FieldString $ pack marketId)
    pairField (FieldString $ pack pair)
    resolution
    where
      qparams = queryParams db
        & manager .~ Right mgr
        & precision .~ Millisecond
      db = "cryptounicorns"
      measurementName = "ticker"
      field = "last"
      marketField = "market"
      pairField = "currency-pair"

data TickerData = TickerData {
  marketId :: String,
  pair :: String,
  history :: V.Vector (Int64, Double)
} deriving (Eq, Show, Generic)

instance ToJSON TickerData
instance FromJSON TickerData

data TickerHistoryPoint = TickerHistoryPoint {
  time :: POSIXTime,
  price :: Double
} deriving (Eq, Show, Generic)

instance ToJSON TickerHistoryPoint
instance FromJSON TickerHistoryPoint

instance QueryResults TickerHistoryPoint where
  parseResults prec' = parseResultsWith $ \_ _ columns fields -> do
    time <- getField "time" columns fields >>= parsePOSIXTime prec'
    FieldFloat price <- getField "last" columns fields >>= parseQueryField
    return TickerHistoryPoint{..}
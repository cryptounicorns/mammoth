{-# LANGUAGE UnicodeSyntax #-}

module Mammoth.Time
  ( getPOSIXMilliseconds
  , getLimitedRange
  , getWeekLimitedRange
  , fromMilliseconds
  , resolveRange
  ) where
import           Data.Time.Clock       (UTCTime)
import           Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)

getPOSIXMilliseconds ∷ IO Integer
getPOSIXMilliseconds = round <$> (* 1000) <$> getPOSIXTime

getLimitedRange ∷ Integer → (Maybe Integer, Maybe Integer) → IO (UTCTime, UTCTime)
getLimitedRange limit range = do
  (from, to) ← resolveRange limit range
  let limitedTo = min (from + limit) to
  return (fromMilliseconds from, fromMilliseconds limitedTo)

getWeekLimitedRange ∷ (Maybe Integer, Maybe Integer) → IO (UTCTime, UTCTime)
getWeekLimitedRange = getLimitedRange $ 7 * 24 * 60 * 60 * 1000

fromMilliseconds ∷ Integer → UTCTime
fromMilliseconds = posixSecondsToUTCTime . (/ 1000) . fromInteger

resolveRange ∷ Integer → (Maybe Integer, Maybe Integer) → IO (Integer, Integer)
resolveRange _ (Just from, Just to) = return (from, to)
resolveRange limit _ = getPOSIXMilliseconds >>= \now → return (now - limit, now)

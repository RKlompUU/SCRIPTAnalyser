
-- | Dealing with Unix timestamps

{-# LANGUAGE PackageImports #-}
module Bitcoin.Misc.UnixTime where

--------------------------------------------------------------------------------

import Data.Word

import "time" Data.Time
import "time" Data.Time.Clock.POSIX

-- import "old-locale" System.Locale

--------------------------------------------------------------------------------

newtype UnixTimeStamp = UnixTimeStamp { fromUnixTimeStamp :: Word32 } deriving (Eq,Ord)

instance Show UnixTimeStamp where 
  show ts = "UnixTimeStamp<" ++ isoDateStr ts ++ ">" 

unixTimeStampToUTC :: UnixTimeStamp -> UTCTime
unixTimeStampToUTC (UnixTimeStamp unixstamp) = utctime where
  ndifftime = fromIntegral unixstamp :: POSIXTime   -- type POSIXTime = NominalDiffTime
  utctime   = posixSecondsToUTCTime ndifftime

isoDateStr :: UnixTimeStamp -> String
isoDateStr unixstamp = formatTime locale "%Y-%m-%d %H:%M:%S" utctime where
  utctime = unixTimeStampToUTC unixstamp
  locale  = defaultTimeLocale

--------------------------------------------------------------------------------

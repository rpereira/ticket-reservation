{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Schedule where

import Data.Maybe                  (Maybe, listToMaybe)
import Data.Text                   (Text)
import Database.Persist.Postgresql (Entity (..), selectList, selectFirst, (==.), (>.))
import Servant
import Data.Time.Clock.POSIX
import Data.Time            (UTCTime)
-- import Data.Time.Clock.UTC

import Config (App (..), Config (..))
import Models

epochToUTC :: Integer -> UTCTime
epochToUTC t = posixSecondsToUTCTime $ fromInteger t / 1000

type ScheduleAPI =
       "schedules" :> Get '[JSON] [Entity Schedule]
  :<|> "schedules" :> Capture "crs" Text
                   :> Get '[JSON] [Entity Schedule]
  :<|> "schedules" :> Capture "time" UTCTime
                   :> Get '[JSON] [Entity Schedule]

-- | The server that runs the ScheduleAPI
scheduleServer :: ServerT ScheduleAPI App
scheduleServer = allSchedules :<|> getScheduleForStation :<|> getScheduleForStation'

allSchedules :: App [Entity Schedule]
allSchedules = runDb (selectList [] [])

getScheduleForStation :: Text -> App [Entity Schedule]
getScheduleForStation crsCode = runDb $ do
  station <- selectFirst [StationCrsCode ==. crsCode] []
  case station of
    Nothing -> return []
    Just s -> selectList [ScheduleStationId ==. entityKey s] []

getScheduleForStation' :: UTCTime -> App [Entity Schedule]
getScheduleForStation' time = runDb $ do
  station <- selectFirst [StationCrsCode ==. "LBG"] []
  case station of
    Nothing -> return []
    Just s -> selectList [ScheduleStationId ==. entityKey s,
                          ScheduleDepartureTime >. time] []

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Schedule where

import Data.Maybe                  (Maybe, listToMaybe)
import Data.Text                   (Text)
import Database.Persist.Postgresql (Entity (..), selectList, selectFirst, (==.))
import Servant

import Config (App (..), Config (..))
import Models

type ScheduleAPI =
       "schedules" :> Get '[JSON] [Entity Schedule]
  :<|> "schedules" :> Capture "crs" Text :> Get '[JSON] [Entity Schedule]

-- | The server that runs the ScheduleAPI
scheduleServer :: ServerT ScheduleAPI App
scheduleServer = allSchedules :<|> getScheduleForStation

allSchedules :: App [Entity Schedule]
allSchedules = runDb (selectList [] [])

getScheduleForStation :: Text -> App [Entity Schedule]
getScheduleForStation crsCode = runDb $ do
  station <- selectFirst [StationCrsCode ==. crsCode] []
  case station of
    Nothing -> return []
    Just s -> selectList [ScheduleStationId ==. entityKey s] []

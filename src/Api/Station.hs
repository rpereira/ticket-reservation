{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api.Station where

import Database.Persist.Postgresql (Entity (..), selectList, selectFirst, (==.),
                                    (>=.))
import Data.Text                   (Text)
import Data.Time                   (UTCTime)
import Servant

import Config                      (App (..), Config (..))
import Models

type StationAPI =
       "stations" :> Get '[JSON] [Entity Station]
  :<|> "stations" :> Capture "crs" Text
                  :> "timetable"
                  :> Get '[JSON] [Entity Schedule]
  :<|> "stations" :> Capture "crs" Text
                  :> "timetable"
                  :> Capture "time" UTCTime
                  :> Get '[JSON] [Entity Schedule]

stationServer :: ServerT StationAPI App
stationServer =
       allStations
  :<|> scheduleForStation
  :<|> scheduleForStationAfterTime

allStations :: App [Entity Station]
allStations = runDb (selectList [] [])

scheduleForStation :: Text -> App [Entity Schedule]
scheduleForStation crsCode = runDb $ do
  station <- selectFirst [StationCrsCode ==. crsCode] []
  case station of
    Nothing -> return []
    Just s -> selectList [ScheduleStationId ==. entityKey s] []

scheduleForStationAfterTime :: Text -> UTCTime -> App [Entity Schedule]
scheduleForStationAfterTime crs time = runDb $ do
  station <- selectFirst [StationCrsCode ==. crs] []
  case station of
    Nothing -> return []
    Just s -> selectList [ScheduleStationId ==. entityKey s,
                          ScheduleDepartureTime >=. Just time] []

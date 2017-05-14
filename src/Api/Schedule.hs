{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api.Schedule where

import Data.Text                   (Text)
import Database.Persist.Postgresql (Entity (..), selectList, selectFirst, (==.))
import Servant

import Config (App (..), Config (..))
import Models

type ScheduleAPI = "schedules" :> Get '[JSON] [Entity Schedule]

-- | The server that runs the ScheduleAPI
scheduleServer :: ServerT ScheduleAPI App
scheduleServer = allSchedules

allSchedules :: App [Entity Schedule]
allSchedules = runDb (selectList [] [])

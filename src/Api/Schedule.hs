{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Schedule where

import Data.Maybe                  (Maybe, listToMaybe)
import Data.Text                   (Text)
import Database.Persist.Postgresql (Entity (..), selectList)
import Servant

import Config                      (App (..), Config (..))
import Models

type ScheduleAPI = "schedules" :> Get '[JSON] [Entity Schedule]

-- | The server that runs the ScheduleAPI
scheduleServer :: ServerT ScheduleAPI App
scheduleServer = allSchedules

allSchedules :: App [Entity Schedule]
allSchedules = runDb (selectList [] [])

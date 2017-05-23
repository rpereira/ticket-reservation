{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Schedule where

import Data.Maybe                  (Maybe, fromMaybe)
import Data.Text                   (Text)
import Database.Persist.Postgresql (Entity (..), selectList)
import Database.Persist.Types      (SelectOpt (..))
import Servant

import Config                      (App (..), Config (..))
import Models

-- | Always limit response to this many results.
resultsPerPage = 30

type ScheduleAPI = "schedules" :> QueryParam "page" Int
                               :> Get '[JSON] [Entity Schedule]

-- | The server that runs the ScheduleAPI
scheduleServer :: ServerT ScheduleAPI App
scheduleServer = allSchedules

allSchedules :: Maybe Int -> App [Entity Schedule]
allSchedules pageNumber = do
  let page = fromMaybe 1 pageNumber
  runDb (selectList
    []
    [ LimitTo resultsPerPage,
      OffsetBy $ (page - 1) * resultsPerPage
    ])

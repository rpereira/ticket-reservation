{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api.Train where

import Config (App (..), Config (..))
import Models

import Database.Persist.Postgresql (Entity (..), selectList)
import Servant

type TrainAPI = "trains" :> Get '[JSON] [Entity Train]

-- | The server that runs the TrainAPI
trainServer :: ServerT TrainAPI App
trainServer = allTrains

allTrains :: App [Entity Train]
allTrains = runDb (selectList [] [])

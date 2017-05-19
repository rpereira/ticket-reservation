{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api.Train where

import Data.Text                   (Text)
import Database.Persist.Postgresql (Entity (..), selectList, selectFirst, (==.))
import Servant

import Config                      (App (..), Config (..))
import Models

type TrainAPI =
       "trains" :> Get '[JSON] [Entity Train]
  :<|> "trains" :> Capture "name" Text :> Get '[JSON] (Entity Train)

-- | The server that runs the TrainAPI
trainServer :: ServerT TrainAPI App
trainServer = allTrains :<|> getTrain

allTrains :: App [Entity Train]
allTrains = runDb (selectList [] [])

getTrain :: Text -> App (Entity Train)
getTrain str = do
  maybeTrain <- runDb (selectFirst [TrainName ==. str] [])
  case maybeTrain of
    Just train -> return train
    Nothing    -> throwError err404

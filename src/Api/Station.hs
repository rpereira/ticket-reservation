{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api.Station where

import Database.Persist.Postgresql (Entity (..), selectList, selectFirst, (==.))
import Servant

import Config (App (..), Config (..))
import Models

type StationAPI =
       "stations" :> Get '[JSON] [Entity Station]
  :<|> "stations" :> Capture "name" String :> Get '[JSON] (Entity Station)

stationServer :: ServerT StationAPI App
stationServer = allStations :<|> getStation

allStations :: App [Entity Station]
allStations = runDb (selectList [] [])

getStation :: String -> App (Entity Station)
getStation str = do
  maybeStation <- runDb (selectFirst [StationName ==. str] [])
  case maybeStation of
    Just station -> return station
    Nothing      -> throwError err404

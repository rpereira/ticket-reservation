{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Reservation where

import Data.Aeson                  (FromJSON(..), ToJSON(..))
import Data.Int                    (Int64)
import Database.Persist.Postgresql (Entity (..), fromSqlKey, selectKeys, insert, selectList,
                                    getBy, selectFirst, rawSql, (==.), (>=.))
import Data.Text                   (Text)
import Data.Time                   -- (UTCTime, fromGregorian)
import GHC.Generics                (Generic)
import Servant

import Config                      (App (..), Config (..))
import Models

-- data ReqReservation = ReqReservation
--   { tainName      :: Text
--   , passengerName :: Text
--   , dpTime        :: UTCTime
--   , srcStId       :: Int
--   , dstStId       :: Int
--   }
--   deriving (Show, Generic)
--
-- instance FromJSON ReqReservation
-- instance ToJSON ReqReservation

type ReservationAPI =
       "reservation" :> "seats"
                     :> Capture "train" Text
                     :> Get '[JSON] [Entity TrainSeats]

  -- :<|> "reservation" :> ReqBody '[JSON] Reservation :> Post '[JSON] Int64
  -- :<|> "reservation" :> ReqBody '[JSON] ReqReservation :> Post '[JSON] Int64

reservationServer :: ServerT ReservationAPI App
reservationServer = availableSeats
               -- :<|> makeReservation

availableSeats :: Text -> App [Entity TrainSeats]
availableSeats trainName =
  runDb $ rawSql s []
        where s = "SELECT * \
                  \FROM train_seats INNER JOIN seat_reservation \
                  \ON train_seats.id = seat_reservation.id"

-- makeReservation :: ReqReservation -> App Int64
-- makeReservation :: Reservation -> App Int64
-- makeReservation req = do
--   -- TODO: get trainId by the given name
--   -- let trainId = selectKeys [TrainName ==. "G47991"]
--   let trainId = runDb (getBy $ TrainCode "G47991")
--   let dpTime = UTCTime (fromGregorian 2017 5 26) 0
--   newReservation <- runDb (
--     insert (Reservation trainId "Rui Pereira" dpTime 132 245))
--   return $ fromSqlKey newReservation

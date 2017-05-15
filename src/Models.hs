{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import Control.Monad.Reader
import Data.Aeson           (FromJSON, ToJSON)
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Database.Persist.Sql
import Database.Persist.TH  (mkMigrate, mkPersist, persistLowerCase,
                             share, sqlSettings)
import GHC.Generics         (Generic)

import Config

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Schedule json
  train_id       Int
  station_id     Int
  src_station_id Int
  dst_station_id Int
  arrival_time   UTCTime Maybe
  departure_time UTCTime Maybe
  pass_time      UTCTime Maybe
  service        Text sqltype=varchar(8)

  deriving Show

Station json
  name        Text
  crs_code    Text sqltype=varchar(3)
  tiplic_code Text sqltype=varchar(7)

  UniqueCode crs_code tiplic_code

  deriving Show

Train json
  name Text
  deriving Show
|]

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool

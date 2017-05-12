{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api (app) where

import Control.Monad.Except
import Control.Monad.Reader        (ReaderT, runReaderT)
import Control.Monad.Reader.Class
import Network.Wai                 (Application)
import Servant

import Config                      (App (..), Config (..))
import Models

import Api.Train

-- | This is the function we export to run our 'trainAPI'. Given a 'Config', we
-- return a WAI 'Application' which any WAI compliant server can run.
trainApp :: Config -> Application
trainApp cfg = serve (Proxy :: Proxy TrainAPI) (appToServer cfg)

-- | This functions tells Servant how to run the 'App' monad with our 'server'
-- function.
appToServer :: Config -> Server TrainAPI
appToServer cfg = enter (convertApp cfg) trainServer

-- | This function converts our 'App' monad into the @ExceptT ServantErr IO@
-- monad that Servant's 'enter' function needs in order to run the application.
-- The ':~>' type is a natural transformation, or, in non-category theory terms,
-- a function that converts two type constructors without looking at the values
-- in the types.
convertApp :: Config -> App :~> ExceptT ServantErr IO
convertApp cfg = Nat (flip runReaderT cfg . runApp)

type API = TrainAPI

appApi :: Proxy API
appApi = Proxy

app :: Config -> Application
app cfg = serve appApi (appToServer cfg)

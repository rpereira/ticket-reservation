{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

data Train = Train
  { id :: Int
  , name :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Train)

type API = "trains" :> Get '[JSON] [Train]

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return trains

trains :: [Train]
trains = [ Train 1 "Alpha Train"
         , Train 2 "Beta Train"
         ]

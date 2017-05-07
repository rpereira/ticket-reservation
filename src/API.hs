{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module API where

import Models

import Servant

type API = "trains" :> Get '[JSON] [Train]

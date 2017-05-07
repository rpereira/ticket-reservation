{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Models where

import Data.Aeson
import Data.Aeson.TH

data Train = Train
  { id :: Int
  , name :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Train)

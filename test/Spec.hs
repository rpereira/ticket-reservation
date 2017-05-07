{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib (app)

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (return app) $ do
  describe "GET /trains" $ do
    it "responds with 200" $ do
      get "/trains" `shouldRespondWith` 200
    it "responds with [Train]" $ do
      let trains = "[{\"id\":1,\"name\":\"Alpha Train\"},{\"id\":2,\"name\":\"Beta Train\"}]"
      get "/trains" `shouldRespondWith` trains

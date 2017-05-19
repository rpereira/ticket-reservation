{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api.User where

import Data.Int                    (Int64)
import Data.Text                   (Text)
import Database.Persist.Postgresql (Entity (..), fromSqlKey, insert, selectList,
                                    selectFirst, (==.))
import Servant

import Config (App (..), Config (..))
import Models

type UserAPI =
       "users" :> Get '[JSON] [Entity User]
  :<|> "users" :> Capture "name" Text :> Get '[JSON] (Entity User)
  :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] Int64

userServer :: ServerT UserAPI App
userServer = allUsers :<|> singleUser :<|> createUser

allUsers :: App [Entity User]
allUsers = runDb (selectList [] [])

singleUser :: Text -> App (Entity User)
singleUser str = do
  maybeUser <- runDb (selectFirst [UserName ==. str] [])
  case maybeUser of
    Nothing -> throwError err404
    Just person -> return person

createUser :: User -> App Int64
createUser p = do
  newUser <- runDb (insert (User (userName p) (userEmail p)))
  return $ fromSqlKey newUser

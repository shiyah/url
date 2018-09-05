{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Servant
import Data.Text
import Data.Text.Read
import Data.Text.Encoding
import DbHandler
import Database.Persist
import Database.Persist.Sqlite
import Model


type UrlAPI = "url" :> ("new" :> ReqBody '[JSON] Text :> Post '[JSON] (Key Url))
                    :<|> "get" :> (Capture "shortened" Text :> Get '[JSON] Text)
                    :<|> Get '[JSON] [Entity Url]

postUrl :: Text -> App (Key Url)
postUrl l = do maybeLink <- queryDb (getBy $ UniqueUrl l)
               case maybeLink of Just _  -> throwError err409
                                 Nothing -> queryDb (insert $ Url l)

getUrl :: Text -> App Text
getUrl sl = do let dbid = decimal sl
               case dbid of Right (a, _) -> do l <- queryDb $ get $ toSqlKey a
                                               case l of Just b -> throwError $ err302 { errHeaders = [("Location", encodeUtf8 $ urlLink b)] }
                                                         Nothing -> throwError err404
                            Left _       -> throwError err400

getUrlsList :: App [Entity Url]
getUrlsList = queryDb (selectList [] [])

urlServer :: ServerT UrlAPI App
urlServer = postUrl :<|> getUrl :<|> getUrlsList

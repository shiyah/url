{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Main where
import Servant
import Data.Text ()
import Network.Wai.Handler.Warp (run)
import Control.Monad.Reader (runReaderT)
import Database.Persist.Sql (runSqlPool)

import Model
import DbHandler
import Lib


main :: IO ()
main = do let env = Production
          let port = 8080
          pool <- runSqliteDb env "urls"
          let cfg = DbConfig { getPool = pool, getEnv = env }
              logger = setLogger env
          runSqlPool (changeSchema [mUrls]) pool
          run port $ logger $ urlApp cfg

-- | Helper function that creates the Application
urlApp :: DbConfig -> Application
urlApp cfg = serve api (hoistServer api (convertApp cfg) urlServer)
             where api = Proxy :: Proxy UrlAPI

-- | A utility function to create the Handler for hoistServer
convertApp :: DbConfig -> App a -> Handler a
convertApp c app = runReaderT (runApp app) c
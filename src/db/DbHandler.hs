{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module DbHandler where
import           Control.Monad.Except                 (MonadError)
import           Control.Monad.Logger                 (LoggingT,
                                                       runStderrLoggingT,
                                                       runStdoutLoggingT)
import           Control.Monad.Reader                 (MonadIO, MonadReader,
                                                       ReaderT, asks, liftIO,
                                                       void)
import           Data.Pool
import           Data.Text
import           Database.Persist.Sqlite
import qualified Database.Sqlite                      as Sqlite
import           Network.Wai                          (Middleware)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           Servant                              (Handler, ServantErr)

import           Model                                ()

{-|
This type contains all of the Monads needed to combine the functionality of the webserver and database
-}
newtype App a = App { runApp :: ReaderT DbConfig Handler a }
                      deriving ( Functor,
                                 Applicative,
                                 Monad,
                                 MonadReader DbConfig,
                                 MonadError ServantErr,
                                 MonadIO)

-- | A data record that's passed to functions that involve the database
data DbConfig = DbConfig { getPool :: ConnectionPool,
                           getEnv  :: Environment
                         }

-- | Describes the types of environments available
data Environment = Development
                   | Test
                   | Production
                   deriving (Eq, Show, Read)

setLogger :: Environment -> Middleware
setLogger Test        = id
setLogger Development = logStdoutDev
setLogger Production  = logStdout

envPool :: Environment -> Int
envPool Test        = 1
envPool Development = 1
envPool Production  = 8

-- | Returns the database file name to use
poolName :: Environment -> Text -> Text
poolName Test        n = append n "testdb.db3"
poolName Development n = append n "dev.db3"
poolName Production  n = append n "data.db3"

envLogger :: (MonadIO m) => Environment -> LoggingT m a -> m a
envLogger Test        = runStdoutLoggingT
envLogger Development = runStdoutLoggingT
envLogger Production  = runStderrLoggingT

-- | SQLite does not enable foreign keys by default
enableForeignKeys :: Sqlite.Connection -> IO ()
enableForeignKeys conn = Sqlite.prepare conn "PRAGMA foreign_keys = ON;" >>= void . Sqlite.step

-- | Creates the SQLite server
createSqliteBackend :: Text -> LogFunc -> IO SqlBackend
createSqliteBackend connStr logFunc = do conn <- Sqlite.open connStr
                                         enableForeignKeys conn
                                         wrapConnection conn logFunc

-- | Runs the SQLite server
runSqliteDb :: Environment -> Text -> IO (Pool SqlBackend)
runSqliteDb e n = envLogger e $ createSqlPool (createSqliteBackend (poolName e n)) (envPool e)

-- | Migrates the database to a new schema
changeSchema :: [Migration] -> ReaderT SqlBackend IO ()
changeSchema = mapM_ runMigration
changeSchema' :: Migration -> ReaderT SqlBackend IO ()
changeSchema' = runMigration

-- | Forcibly migrates the database to a new schema
resetDb :: Migration -> ReaderT SqlBackend IO ()
resetDb = runMigrationUnsafe

-- | Runs a query in the context of certain necessary monads
queryDb :: (MonadReader DbConfig m, MonadIO m) => SqlPersistT IO b -> m b
queryDb query = do pool <- asks getPool
                   liftIO $ runSqlPool query pool

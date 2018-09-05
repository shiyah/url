{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model where
import Data.Text (Text)
import Database.Persist.TH (persistLowerCase, share, mkPersist, sqlSettings, mkMigrate)

share [mkPersist sqlSettings, mkMigrate "mUrls"] [persistLowerCase|
  Url json
    link Text

    UniqueUrl link
    deriving Show Read Eq
|]
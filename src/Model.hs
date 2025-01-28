{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Model where

import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Greeting
    message String
    deriving Show
|]

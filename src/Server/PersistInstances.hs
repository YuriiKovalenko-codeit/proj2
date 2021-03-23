{-# LANGUAGE OverloadedStrings #-}

module Server.PersistInstances where

import Data.Password
import Data.Text
import Data.UUID
import Database.Persist.Class
import Database.Persist.Sql

instance PersistField UUID where
  toPersistValue = toPersistValue . toText
  fromPersistValue v = case fromPersistValue v of
    Right text -> case fromText text of
      Just uuid -> Right uuid
      Nothing -> Left $ "Failed to parse UUID value: " `append` text
    _ -> Left $ pack $ "Expected PersistText, got: " ++ show v

instance PersistFieldSql UUID where
  sqlType _ = SqlString

instance PersistField PassHash where
  toPersistValue = toPersistValue . unPassHash
  fromPersistValue v =
    either
      (const $ Left $ pack $ "Expected PersistText, got: " ++ show v)
      (Right . PassHash)
      $ fromPersistValue v

instance PersistFieldSql PassHash where
  sqlType _ = SqlString

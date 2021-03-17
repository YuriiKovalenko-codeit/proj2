{-# LANGUAGE OverloadedStrings #-}
module Server.PersistInstances where

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
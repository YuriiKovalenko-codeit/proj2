{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Endpoints.Register where

import API.Types
import Servant.Server
import Server.DB
import Server.Helpers
import Server.Types

register :: RegisterRequest -> HandlerT ProfileInfo
register (RegRequest login password) =
  maybe (reportHandlerError err409 "confliciting registration") return
    =<< runOnPool
      ( do
          exists <- userWithLoginExists login
          if exists
            then return Nothing
            else do
              uuid <- createUser login (hashPassword password)
              return $ Just $ ProfileInfo login uuid
      )
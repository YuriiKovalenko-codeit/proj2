module Server.Endpoints.UpdateProfile where

import API.Types
import AuthenticatedUser
import Database.Persist
import Servant.API
import Servant.Auth.Server
import Servant.Server
import Server.DB
import Server.Helpers
import Server.Types

updateProfile :: AuthResult AuthenticatedUser -> ProfileUpdateInfo -> HandlerT NoContent
updateProfile (Authenticated (AUser uuid)) updInfo@ProfileUpdInfo {..} =
  maybe (reportHandlerError err409 "conflicting login update") return
    =<< runOnPool
      ( do
          loginConflict <- maybe (return False) userWithLoginExists newLogin
          if loginConflict
            then return Nothing
            else do
              updateWhere [UserUuid ==. uuid] $ getUpdatesList updInfo
              return $ Just NoContent
      )
updateProfile _ _ = reportHandlerError err401 "Unauthorized profile edit attempt"
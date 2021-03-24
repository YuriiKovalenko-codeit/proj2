module API where

import API.Types
import Data.Text
import Servant.API
import qualified Servant.API as SAPI

type API' loginAuth commonAuth =
  "profile" :> Capture "login" Text :> Get '[JSON] ProfileInfo
    :<|> "login" :> loginAuth :> Post '[JSON] LoginResponse
    :<|> "auth-test" :> commonAuth :> Get '[JSON] NoContent
    :<|> "register" :> ReqBody '[JSON] RegisterRequest :> Post '[JSON] ProfileInfo
    :<|> "user" :> commonAuth :> Get '[JSON] ProfileInfo
    :<|> "user" :> commonAuth :> ReqBody '[JSON] ProfileUpdateInfo :> Put '[JSON] NoContent
module IntegrationSpec where

import API.Types
import Client
import Data.Default
import Helpers.DB
import Helpers.Hspec
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant
import Servant.Client
import Test.Hspec

withServerAround :: SpecWith a -> SpecWith a
withServerAround = runAroundAllWait "stack" ["run", "--", "server", "--configPath", testConfigPath] 2

integrationSpec :: Spec
integrationSpec =
  describe "Client-server interaction" $
    withServerAround $
      beforeAll createPool $
        beforeWith restoreDB $ do
          manager' <- runIO $ newManager defaultManagerSettings
          let clientEnv = mkClientEnv manager' $ BaseUrl Http "localhost" 8082 ""

              checkRes (Left e) = expectationFailure (show e) >> undefined
              checkRes (Right r) = return r

              testWithClient action = do
                checkRes =<< runClientM action clientEnv
                return ()

          it "successfully logins" $ do
            testWithClient $ loginUser $ BasicAuthData "user1" "11"

          it "updates the profile" $ do
            testWithClient $ do
              LoginResponse uuid tok <- loginUser $ BasicAuthData "user1" "11"
              updateUser (tokenFromText tok) $ def {newPassword = Just "22"}
              loginUser $ BasicAuthData "user1" "22"

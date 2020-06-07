{-# LANGUAGE DataKinds, TypeOperators #-}

module HttpAPI.FrontEnd
    ( FrontEndAPI(..)
    , frontEndAPIServer
    , ElmSeed(..)
    ) where

import Servant
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON, encode)
import Servant.Auth.Server
import HttpAPI.AuthAPI (UserJWT, un)
import HttpAPI.HTML
import Lucid
import Control.Monad.Except

type FrontEndAPI = Get '[HTML] RawHtml
              :<|> "app" :> "login"     :> Get '[HTML] RawHtml
              :<|> "app" :> "dashboard" :> Get '[HTML] RawHtml
              :<|> "app" :> "learning"  :> Get '[HTML] RawHtml
              :<|> "app" :> "training"  :> Get '[HTML] RawHtml
              :<|> "app" :> "settings"  :> Get '[HTML] RawHtml

frontEndAPIServer :: AuthResult UserJWT -> Server FrontEndAPI
frontEndAPIServer auth = renderPage "Home" "Home" auth
                    :<|> renderPage "Dashboard" "Login" auth
                    :<|> renderPage "Dashboard" "Login" auth
                    :<|> renderPage "Learning" "Login" auth
                    :<|> renderPage "Training" "Login" auth
                    :<|> renderPage "Settings" "Login" auth

data ElmSeed = ElmSeed 
    { page :: !Text
    , user :: !(Maybe Text)
    } deriving (Generic)

instance ToJSON ElmSeed
instance FromJSON ElmSeed

renderPage :: Text
           -> Text
           -> AuthResult UserJWT 
           -> Handler RawHtml
renderPage firstPage _ (Authenticated userJWT) = do
    liftIO $ putStrLn "Logged in!"
    pure $ RawHtml $ renderBS (renderElmApp (ElmSeed firstPage (Just (un userJWT))))
renderPage _ secondPage ar = do
    liftIO $ putStrLn ("NOT logged in " ++ show ar)
    pure $ RawHtml $ renderBS (renderElmApp (ElmSeed secondPage Nothing))
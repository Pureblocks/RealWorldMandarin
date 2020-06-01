{-# LANGUAGE DataKinds, TypeOperators #-}

module HttpAPI.LoginAPI
    ( LoginAPI(..)
    , loginServer
    ) where

import Servant
import Control.Monad.Except
import Database.CharacterDB
import Database.Beam.Postgres (Connection)
import GHC.Generics (Generic)
import Data.Text
import Data.Aeson
import Data.Maybe (maybe)
import Servant.Auth.Server

-- | Body payload for registering a user
data Register = 
    Register
        { registerUsername :: !Text
        , email            :: !Text
        , passwordOne      :: !Text
        , passwordTwo      :: !Text
        } deriving (Eq, Show, Generic)

instance ToJSON Register
instance FromJSON Register

-- | Body payload for logging in
data Login =
    Login
        { username :: !Text
        , password :: !Text
        } deriving (Eq, Show, Generic)

instance ToJSON Login
instance FromJSON Login

newtype LoginRegistrationError =
    LoginRegistrationError
        { error :: Text
        } deriving (Show, Generic)

instance ToJSON LoginRegistrationError
instance FromJSON LoginRegistrationError

data UserJWT = 
    UserJWT
        { _username :: !Text
        , _password :: !Text
        , _email    :: !Text
        } deriving (Generic)

instance FromJSON UserJWT
instance ToJSON UserJWT

instance FromJWT UserJWT
instance ToJWT UserJWT

type NoContentWithLoginCookies = (Headers '[ Header "Set-Cookie" SetCookie
                                            , Header "Set-Cookie" SetCookie]
                                            NoContent)

type LoginAPI = "login"
        :> ReqBody '[JSON] Login
        :> Verb 'POST 204 '[JSON] NoContentWithLoginCookies
    :<|> "register"
        :> ReqBody '[JSON] Register
        :> Verb 'POST 204 '[JSON] NoContentWithLoginCookies

loginServer :: Connection 
            -> CookieSettings
            -> JWTSettings 
            -> Server LoginAPI
loginServer conn cs jwtCfg = checkCredentials conn cs jwtCfg 
                        :<|> registerUser conn cs jwtCfg 

checkCredentials :: Connection 
                 -> CookieSettings
                 -> JWTSettings
                 -> Login 
                 -> Handler NoContentWithLoginCookies
checkCredentials conn cs jwtCfg login = do
    maybeUser <- liftIO $ selectUserForLogin conn (username login) (password login)
    maybe loginError (setCookies cs jwtCfg) maybeUser

loginError :: Handler NoContentWithLoginCookies
loginError = throwError err503 
    { errBody = encode 
        (LoginRegistrationError  "Incorrect username password combination.") 
    }

setCookies :: CookieSettings
           -> JWTSettings
           -> User
           -> Handler NoContentWithLoginCookies
setCookies cs jwtCfg user = do
    let (un, up, ue) = jwtFromUser user
        userJWT      = UserJWT un up ue
    mApplyCookies <- liftIO $ acceptLogin cs jwtCfg userJWT
    case mApplyCookies of
        Nothing -> throwError err401
        Just applyCookies -> return $ applyCookies NoContent

registerPasswordError :: Handler NoContentWithLoginCookies
registerPasswordError = throwError err503 
    { errBody = encode 
        (LoginRegistrationError  "Passwords do not match.") 
    }

registerUser :: Connection 
             -> CookieSettings
             -> JWTSettings 
             -> Register 
             -> Handler NoContentWithLoginCookies
registerUser conn cs jwtCfg register =
    if passwordOne register /= passwordTwo register
        then registerPasswordError
    else do
        user <- liftIO $ registerNewUser conn
                                         (registerUsername register)
                                         (email register)
                                         (passwordOne register)
        setCookies cs jwtCfg user
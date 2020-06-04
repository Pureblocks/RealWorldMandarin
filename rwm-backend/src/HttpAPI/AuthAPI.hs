{-# LANGUAGE DataKinds, TypeOperators, TemplateHaskell #-}

module HttpAPI.AuthAPI
    ( AuthAPI(..)
    , loginServer
    , Register(..)
    , Login(..)
    ) where

import Servant
import Control.Monad.Except
import Database.CharacterDB
import Database.Beam.Postgres (Connection)
import GHC.Generics (Generic)
import Data.Text
import Data.Aeson (FromJSON, ToJSON, encode)
import Data.Maybe (maybe)
import Data.Time.Clock (getCurrentTime, addUTCTime, UTCTime)
import Servant.Auth.Server
import Control.Exception (try, Exception)

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
        { exp      :: !UTCTime -- Experation date
        , sub      :: !Int     -- Subject (user id)
        , un       :: !Text    -- User name
        } deriving (Generic)

instance FromJSON UserJWT
instance ToJSON UserJWT

instance FromJWT UserJWT
instance ToJWT UserJWT

type NoContentAuth = (Headers '[ Header "Set-Cookie" SetCookie
                               , Header "Set-Cookie" SetCookie
                               ]
                               NoContent )

type AuthAPI = 
    "api" :> "auth" :> "login"
            :> ReqBody '[JSON] Login
            :> Verb 'POST 204 '[JSON] NoContentAuth
        :<|> 
    "api" :> "auth" :> "register"
            :> ReqBody '[JSON] Register
            :> Verb 'POST 204 '[JSON] NoContentAuth

loginServer :: Connection 
            -> CookieSettings
            -> JWTSettings 
            -> Server AuthAPI
loginServer conn cs jwtCfg = checkCredentials conn cs jwtCfg 
                        :<|> registerUser conn cs jwtCfg 

checkCredentials :: Connection 
                 -> CookieSettings
                 -> JWTSettings
                 -> Login 
                 -> Handler NoContentAuth
checkCredentials conn cs jwtCfg login = do
    maybeUser <- liftIO $ selectUserForLogin conn (username login) (password login)
    maybe loginError (setCookies cs jwtCfg) maybeUser

loginError :: Handler NoContentAuth
loginError = throwError err503 
    { errBody = encode 
        (LoginRegistrationError  "Incorrect username password combination.") 
    }

setCookies :: CookieSettings
           -> JWTSettings
           -> User
           -> Handler NoContentAuth
setCookies cs jwtCfg user = do
    exp           <- liftIO getCurrentTime
    let (sub, un) = jwtFromUser user
        userJWT   = UserJWT (addUTCTime 1.21e+6 exp) sub un
    mApplyCookies <- liftIO $ acceptLogin cs jwtCfg userJWT
    case mApplyCookies of
        Nothing -> throwError err401
        Just applyCookies -> return $ applyCookies NoContent

registerPasswordError :: Handler NoContentAuth
registerPasswordError = throwError err400
    { errBody = encode 
        (LoginRegistrationError  "Passwords do not match.") 
    }

registerUser :: Connection 
             -> CookieSettings
             -> JWTSettings 
             -> Register 
             -> Handler NoContentAuth
registerUser conn cs jwtCfg register =
    if passwordOne register /= passwordTwo register
        then registerPasswordError
    else do
        userOrError <- liftIO (try (registerNewUser 
                        conn
                        (registerUsername register)
                        (email register)
                        (passwordOne register)) :: IO (Either ViolationError User))
        case userOrError of
            Right user -> setCookies cs jwtCfg user
            Left ve    -> uniqueViolationErrorHandler ve

uniqueViolationErrorHandler :: ViolationError -> Handler NoContentAuth
uniqueViolationErrorHandler (ViolationError e) = throwError err400
    { errBody = encode 
        (LoginRegistrationError e) 
    }

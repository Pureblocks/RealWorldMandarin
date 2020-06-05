{-# LANGUAGE DataKinds, TypeOperators, TemplateHaskell #-}

module HttpAPI.AuthAPI
    ( AuthAPI(..)
    , loginServer
    , Register(..)
    , Login(..)
    , UserLogin(..)
    ) where

import Servant
import Control.Monad.Except
import Database.CharacterDB
import Database.Beam.Postgres (Connection)
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.ByteString.Char8 (pack)
import Data.CaseInsensitive  (mk)
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
        { loginUsername :: !Text
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

newtype UserLogin =
    UserLogin
        { username :: Text
        } deriving (Generic)

instance FromJSON UserLogin
instance ToJSON UserLogin

type WithCookie a = Headers '[ Header "Set-Cookie" SetCookie
                                 , Header "Set-Cookie" SetCookie
                                 ]
                                 NoContent

type AuthAPI = 
    "api" :> "auth" :> "login"
            :> ReqBody '[JSON] Login
            :> Verb 'POST 200 '[JSON] (WithCookie UserLogin)
        :<|> 
    "api" :> "auth" :> "register"
            :> ReqBody '[JSON] Register
            :> Verb 'POST 201 '[JSON] (WithCookie UserLogin)

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
                 -> Handler (WithCookie UserLogin)
checkCredentials conn cs jwtCfg login = do
    maybeUser <- liftIO $ selectUserForLogin conn (loginUsername login) (password login)
    maybe loginError (setCookies cs jwtCfg) maybeUser

loginError :: Handler (WithCookie UserLogin)
loginError = throwError err400 
    { errBody = encode 
        (LoginRegistrationError  "Incorrect loginUsername password combination.")
    , errHeaders = [( mk $ pack "Content-Type"
                    , pack "application/json;charset=utf-8")]
    }

setCookies :: CookieSettings
           -> JWTSettings
           -> User
           -> Handler (WithCookie UserLogin)
setCookies cs jwtCfg user = do
    exp           <- liftIO getCurrentTime
    let (sub, un) = jwtFromUser user
        userJWT   = UserJWT (addUTCTime 1.21e+6 exp) sub un
    mApplyCookies <- liftIO $ acceptLogin cs jwtCfg userJWT
    case mApplyCookies of
        Nothing -> throwError err401
        Just applyCookies -> return $ applyCookies NoContent -- (UserLogin (userNameFromUser user))

registerPasswordError :: Handler (WithCookie UserLogin)
registerPasswordError = throwError err400
    { errBody = encode 
        (LoginRegistrationError  "Passwords do not match.") 
    , errHeaders = [( mk $ pack "Content-Type"
                    , pack "application/json;charset=utf-8")]
    }

registerUser :: Connection 
             -> CookieSettings
             -> JWTSettings 
             -> Register 
             -> Handler (WithCookie UserLogin)
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

uniqueViolationErrorHandler :: ViolationError -> Handler (WithCookie UserLogin)
uniqueViolationErrorHandler (ViolationError e) = throwError err400
    { errBody = encode 
        (LoginRegistrationError e)
    , errHeaders = [( mk $ pack "Content-Type"
                    , pack "application/json;charset=utf-8")]
    }

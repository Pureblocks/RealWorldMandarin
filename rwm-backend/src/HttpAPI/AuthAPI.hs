{-# LANGUAGE DataKinds, TypeOperators #-}

module HttpAPI.AuthAPI
    ( AuthAPI(..)
    , loginServer
    , Register(..)
    , Login(..)
    , UserJWT(..)
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
        , password      :: !Text
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
        { sub      :: !Int     -- Subject (user id)
        , un       :: !Text    -- User name
        } deriving (Generic, Show)

instance FromJSON UserJWT
instance ToJSON UserJWT

instance FromJWT UserJWT
instance ToJWT UserJWT

type WithCookieNoContent = Headers '[ Header "Set-Cookie" SetCookie
                                    , Header "Set-Cookie" SetCookie
                                    ] NoContent

type AuthAPI = 
    "api" :> "auth" :> "login"
            :> ReqBody '[JSON] Login
            :> Verb 'POST 204 '[JSON] WithCookieNoContent
        :<|> 
    "api" :> "auth" :> "register"
            :> ReqBody '[JSON] Register
            :> Verb 'POST 204 '[JSON] WithCookieNoContent

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
                 -> Handler WithCookieNoContent
checkCredentials conn cs jwtCfg login = do
    maybeUser <- liftIO $ selectUserForLogin conn (loginUsername login) (password login)
    maybe loginError (setCookies cs jwtCfg) maybeUser

loginError :: Handler WithCookieNoContent
loginError = throwError err400 
    { errBody = encode 
        (LoginRegistrationError  "Incorrect loginUsername password combination.")
    , errHeaders = [( mk $ pack "Content-Type"
                    , pack "application/json;charset=utf-8")]
    }

setCookies :: CookieSettings
           -> JWTSettings
           -> User
           -> Handler WithCookieNoContent
setCookies cs jwtCfg user = do
    let (sub, un) = jwtFromUser user
        userJWT   = UserJWT sub un
    mApplyCookies <- liftIO $ acceptLogin cs jwtCfg userJWT
    case mApplyCookies of
        Nothing -> throwError err401
        Just applyCookies -> return $ applyCookies NoContent

registerPasswordError :: Handler WithCookieNoContent
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
             -> Handler WithCookieNoContent
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

uniqueViolationErrorHandler :: ViolationError -> Handler WithCookieNoContent
uniqueViolationErrorHandler (ViolationError e) = throwError err400
    { errBody = encode 
        (LoginRegistrationError e)
    , errHeaders = [( mk $ pack "Content-Type"
                    , pack "application/json;charset=utf-8")]
    }

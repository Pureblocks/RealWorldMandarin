module Configuration.Config
    ( Config(..)
    , readConfig
    , TwitterConfig(..)
    , PostgresConfig(..)
    ) where

import Data.Text (Text, pack)
import Data.Aeson
import GHC.Generics (Generic)
import Data.Maybe (fromJust)
import Data.Text.Encoding (encodeUtf8Builder)
import Data.ByteString.Builder(toLazyByteString)
import Crypto.JOSE.JWK (JWK)

data Config 
    = Config
        { twitterConfig  :: !TwitterConfig
        , postgresConfig :: !PostgresConfig
        , jwtKey         :: !JWK
        } deriving (Show, Generic)

instance FromJSON Config

data TwitterConfig
    = TwitterConfig
        { apiKey            :: !Text -- oauth_consumer_key
        , apiSecretKey      :: !Text -- oauth_consumer_secret
        , accessToken       :: !Text -- oauth_token
        , accessTokenSecret :: !Text -- oauth_token_secret
        } deriving (Show, Generic)

instance FromJSON TwitterConfig

data PostgresConfig
    = PostgresConfig
        { host     :: !Text 
        , port     :: !Integer
        , user     :: !Text 
        , password :: !Text 
        , database :: !Text -- database to connect to
        } deriving (Show, Generic)

instance FromJSON PostgresConfig

readConfig :: IO Config
readConfig = fromJust . decode . toLazyByteString . encodeUtf8Builder . pack <$> 
    readFile "configuration/config.json"

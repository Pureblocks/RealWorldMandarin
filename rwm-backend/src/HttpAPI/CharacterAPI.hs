{-# LANGUAGE DataKinds, TypeOperators #-}

module HttpAPI.CharacterAPI
    ( Character(..)
    , Element(..)
    , Story(..)
    , CharacterAPI
    , characterServer
    ) where

import Servant
import Servant.Auth.Server
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, encode)
import Database.Beam.Postgres (Connection)
import HttpAPI.AuthAPI (UserJWT)
import Data.ByteString.Char8 (pack)
import Data.CaseInsensitive  (mk)
import Database.CharacterDB (insertLearnedCharacter, selectNextCharacter, selectElementsFor)
import qualified Database.CharacterDB as CharacterDB
import Control.Monad.Except

data Character =
    Character
        { characterId :: !Int
        , hanzi       :: !Text
        , keyword     :: !Text
        , primitive   :: !Bool
        , elements    :: ![Element]
        } deriving (Show, Generic)

instance ToJSON Character
instance FromJSON Character

data Element =
    Element
        { elementCharacterId :: !Int
        , position           :: !Int
        , elemHanzi          :: !Text
        , elementKeyword     :: !Text
        } deriving (Show, Generic)

instance ToJSON Element
instance FromJSON Element

newtype Story =
    Story
        { story :: Text
        } deriving (Show, Generic)

instance ToJSON Story
instance FromJSON Story

newtype UnauthorizedError =
    UnauthorizedError
        { error :: Text
        } deriving (Show, Generic)

unauthorizedError :: UnauthorizedError
unauthorizedError =
    UnauthorizedError "You need to be authenticated to access this resource."

instance FromJSON UnauthorizedError
instance ToJSON UnauthorizedError

type CharacterAPI =
    "api" 
        :> "characters" 
        :> "user" 
        :> Capture "userId" Int 
        :> "next"
        :> Get '[JSON] Character
    :<|>
    "api" 
        :> "characters"
        :> "users"
        :> Capture "userId" Int
        :> Capture "characterId" Int
        :> ReqBody '[JSON] Story
        :> PostNoContent '[JSON] NoContent

characterServer :: Connection
                -> AuthResult UserJWT
                -> Server CharacterAPI
characterServer conn (Authenticated userJWT) =
    getNextCharacter conn :<|> saveStory conn
characterServer conn _ = throwAll err401
    { errBody = encode unauthorizedError
    , errHeaders = [( mk $ pack "Content-Type"
                    , pack "application/json;charset=utf-8")]
    }

getNextCharacter :: Connection -> Int -> Handler Character
getNextCharacter conn userId = do
    nextCharacter <- liftIO $ selectNextCharacter conn userId
    elements      <- liftIO $ selectElementsFor conn nextCharacter
    return $ createCharacter nextCharacter elements

createCharacter :: CharacterDB.Character
                -> [(CharacterDB.Element, CharacterDB.Character)]
                -> Character
createCharacter char elems =
    Character
        (CharacterDB._charId char)
        (CharacterDB._charHanziCharacter char)
        (CharacterDB._charKeyword char)
        (CharacterDB._charPrimitive char)
        elements
    where 
        elements = fmap createElements elems
        createElements (elem, elemChar) =
            Element
                (CharacterDB._charId elemChar)
                (CharacterDB._elementOrderPosition elem)
                (CharacterDB._charHanziCharacter elemChar)
                (CharacterDB._charKeyword elemChar)


saveStory :: Connection -> Int -> Int -> Story -> Handler NoContent
saveStory conn userId characterId (Story story) =
    liftIO $ NoContent <$ insertLearnedCharacter conn userId characterId story
{-# LANGUAGE RankNTypes #-}

module Database.CharacterDB
    ( User(..)
    , insertCharacters
    , selectNextCharacter
    , selectLearnedCharacters
    , selectNotlearnedCharacters
    , selectUserForLogin
    , jwtFromUser
    , registerNewUser
    ) where

import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Extended
import Database.Beam.Backend.SQL.BeamExtensions
import Data.Text (Text)
import qualified Data.Text as T
import Importer.CharacterImporter (CharacterImport(..))
import Data.Functor.Compose (Compose(..))

data CharacterDB f =
    CharacterDB
        { _tableCharacters     :: f (TableEntity CharacterT)
        , _tableElements       :: f (TableEntity ElementT)
        , _tableUsers          :: f (TableEntity UserT)
        , _tableUserCharacters :: f(TableEntity UserCharacterT)
        } deriving (Generic, Database be)

characterDB :: DatabaseSettings be CharacterDB
characterDB = defaultDbSettings

data CharacterT f =
    Character
        { _charId             :: Columnar f Int
        , _charHanziCharacter :: Columnar f Text
        , _charKeyword        :: Columnar f Text
        , _charPrimitive      :: Columnar f Bool
        } deriving (Generic, Beamable)

type Character = CharacterT Identity
type CharacterId = PrimaryKey CharacterT Identity

deriving instance Show Character
deriving instance Eq Character

instance Table CharacterT where
    data PrimaryKey CharacterT f = CharacterId (Columnar f Int) deriving (Generic, Beamable)
    primaryKey = CharacterId . _charId

deriving instance Show (PrimaryKey CharacterT Identity)
deriving instance Eq (PrimaryKey CharacterT Identity)

data ElementT f =
    Element
        { _elementId            :: Columnar f Int
        , _elementCharacter     :: PrimaryKey CharacterT f
        , _elementOrderPosition :: Columnar f Int
        , _elementElement       :: PrimaryKey CharacterT f
        } deriving (Generic, Beamable)

type Element = ElementT Identity
type ElementId = PrimaryKey ElementT Identity

deriving instance Show Element
deriving instance Eq Element

instance Table ElementT where
    data PrimaryKey ElementT f = ElementId (Columnar f Int) deriving (Generic, Beamable)
    primaryKey = ElementId . _elementId

deriving instance Show (PrimaryKey ElementT Identity)
deriving instance Eq (PrimaryKey ElementT Identity)

data UserT f =
    User
        { _userId           :: Columnar f Int
        , _userUsername     :: Columnar f Text
        , _userEmail        :: Columnar f Text
        , _userPasswordHash :: Columnar f Text
        , _userSalt         :: Columnar f Text
        } deriving (Generic, Beamable)

type User = UserT Identity
type UserId = PrimaryKey UserT Identity

deriving instance Show User
deriving instance Eq User

instance Table UserT where
    data PrimaryKey UserT f = UserId (Columnar f Int) deriving (Generic, Beamable)
    primaryKey = UserId . _userId

deriving instance Show (PrimaryKey UserT Identity)
deriving instance Eq (PrimaryKey UserT Identity)

data UserCharacterT f =
    UserCharacter
        { _ucId        :: Columnar f Int
        , _ucUser      :: PrimaryKey UserT f
        , _ucCharacter :: PrimaryKey CharacterT f
        , _ucStory     :: Columnar f Text
        } deriving (Generic, Beamable)

type UserCharacter = UserCharacterT Identity
type UserCharacterId = PrimaryKey UserCharacterT Identity

deriving instance Show UserCharacter
deriving instance Eq UserCharacter

instance Table UserCharacterT where
    data PrimaryKey UserCharacterT f = UserCharacterId (Columnar f Int) deriving (Generic, Beamable)
    primaryKey = UserCharacterId . _ucId

deriving instance Show (PrimaryKey UserCharacterT Identity)
deriving instance Eq (PrimaryKey UserCharacterT Identity)

-- | Inserts all the characters plus their elements that have been imported 
-- from the Character Importer into the database.
insertCharacters :: Connection -> [CharacterImport] -> IO ()
insertCharacters conn = mapM_ (\ci -> insertCharacterT conn ci >>= insertElementT conn ci)

insertCharacterT :: Connection -> CharacterImport -> IO Character
insertCharacterT conn = 
    fmap head .
    runBeamPostgres conn . 
    runInsertReturningList .
    insert (_tableCharacters characterDB) .
    insertValues .
    pure .
    fromCharacterImport

fromCharacterImport :: CharacterImport -> Character
fromCharacterImport ci = Character
    (charId ci)
    (hanziCharacter ci)
    (keyword ci)
    (primitive ci)

insertElementT :: Connection  -> CharacterImport -> Character -> IO ()
insertElementT conn ci cha = do
    e <- elementFromCharacterImport conn cha ci
    runBeamPostgres conn
        (runInsert
        (insert (_tableElements characterDB)
        (insertExpressions (fmap (getCompose . getThis) e))))

-- | This is required to make the type system accept elementFromCharacterImport
-- in later GHC versions look at impredicative polymorphism
newtype Some tag = This { getThis :: forall t. tag t }

elementFromCharacterImport :: Connection -> Character -> CharacterImport -> IO [Some (Compose ElementT (QExpr Postgres))]
elementFromCharacterImport conn cha ci = 
    let elementIdWithIndex = zip [1..] (elements ci)
        characterId = charId ci
        createElement (index, elemId) = do
            elementCha <- selectCharacter conn elemId
            pure $ This $ Compose $ Element
                default_
                (val_ (pk cha))
                (val_ index)
                (val_ (pk elementCha))
    in mapM createElement elementIdWithIndex

selectCharacter :: Connection -> Int -> IO Character
selectCharacter conn characterId = runSelectOne conn $ select $ 
    filter_ (\c -> _charId c ==. val_ characterId) $
    all_ (_tableCharacters characterDB)

-- | Selects the next character that the user has to learn
selectNextCharacter :: Connection -> Int -> IO Character
selectNextCharacter conn userId =
   selectLatestCharacter conn userId >>= selectCharacter conn . (+) 1

selectLatestCharacter :: Connection -> Int -> IO Int
selectLatestCharacter conn userId =
    let getCharacterId (CharacterId id) = id
        maxCharacterUserQ =
            aggregate_ (\uc -> fromMaybe_ 1 (max_ (getCharacterId $ _ucCharacter uc))) $
                filter_ (\uc -> _ucUser uc ==. val_ (UserId userId)) $
                all_ (_tableUserCharacters characterDB)
    in runSelectOne conn (select maxCharacterUserQ)

-- | Selects all the characters that a user has learned (created a story for)
selectLearnedCharacters :: Connection -> Int -> IO [(Character, UserCharacter)]
selectLearnedCharacters conn userId = undefined

-- | Selects all the characters that a user has not learned
selectNotlearnedCharacters :: Connection -> Int -> IO [Character]
selectNotlearnedCharacters conn userId = undefined

-- | Marks a character learned with a story
insertLearnedCharacter :: Connection -> Int -> Int -> Text -> IO ()
insertLearnedCharacter conn userId characterId story = undefined

-- | Selects the user for logging in (apply encryption etc with salt)
selectUserForLogin :: Connection -> Text -> Text -> IO (Maybe User)
selectUserForLogin conn username password = undefined

-- | Simple helper function to extract values required for UserJWT
jwtFromUser :: User -> (Text, Text, Text)
jwtFromUser user = (_userUsername user, _userPasswordHash user, _userEmail user)

-- | Inserts a new user into the database
-- can throw error based on unique constraints
-- TODO: how to handle these unique constraints errors?
registerNewUser :: Connection -> Text -> Text -> Text -> IO User
registerNewUser conn username email password = undefined
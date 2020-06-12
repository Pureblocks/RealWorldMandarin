{-# LANGUAGE RankNTypes #-}

module Database.CharacterDB
    ( User(..)
    , insertCharacters
    , selectNextCharacter
    , selectLearnedCharacters
    , selectNotlearnedCharacters
    , selectUserForLogin
    , jwtFromUser
    , userNameFromUser
    , registerNewUser
    , ViolationError(..)
    , insertLearnedCharacter
    , selectElementsFor
    , Character(..)
    , CharacterT(..)
    , Element(..)
    , ElementT(..)
    ) where

import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Extended
import Database.Beam.Backend.SQL.BeamExtensions
import Data.Text (Text)
import qualified Data.Text as T
import Importer.CharacterImporter (CharacterImport(..))
import Data.Functor.Compose (Compose(..))
import Crypto.KDF.PBKDF2 (generate, prfHMAC, Parameters(..))
import Crypto.Hash.Algorithms (SHA256(..))
import Crypto.Random (getSystemDRG, SystemDRG, randomBytesGenerate)
import Data.ByteArray.Encoding (convertToBase, Base(Base64))
import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Database.PostgreSQL.Simple.Errors (catchViolation, ConstraintViolation, ConstraintViolation(..))
import Database.PostgreSQL.Simple.Internal (SqlError)
import Control.Exception (try, throwIO, Exception)

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
    data PrimaryKey CharacterT f = CharacterId (Columnar f Int)
                                    deriving (Generic, Beamable)
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
    data PrimaryKey ElementT f = ElementId (Columnar f Int)
                                    deriving (Generic, Beamable)
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
    data PrimaryKey UserT f = UserId (Columnar f Int) 
                                deriving (Generic, Beamable)
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
    data PrimaryKey UserCharacterT f = UserCharacterId (Columnar f Int) 
                                        deriving (Generic, Beamable)
    primaryKey = UserCharacterId . _ucId

deriving instance Show (PrimaryKey UserCharacterT Identity)
deriving instance Eq (PrimaryKey UserCharacterT Identity)

-- | Inserts all the characters plus their elements that have been imported 
-- from the Character Importer into the database.
insertCharacters :: Connection -> [CharacterImport] -> IO ()
insertCharacters conn =
    mapM_ (\ci -> insertCharacterT conn ci >>= insertElementT conn ci)

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

elementFromCharacterImport :: Connection 
                           -> Character
                           -> CharacterImport
                           -> IO [Some (Compose ElementT (QExpr Postgres))]
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
            aggregate_ (\uc -> fromMaybe_ 0 (max_ (getCharacterId $ _ucCharacter uc))) $
                filter_ (\uc -> _ucUser uc ==. val_ (UserId userId)) $
                all_ (_tableUserCharacters characterDB)
    in runSelectOne conn (select maxCharacterUserQ)

selectElementsFor :: Connection -> Character -> IO [(Element, Character)]
selectElementsFor conn character =
    runBeamPostgres conn $
        runSelectReturningList $ select $
        do elements <- 
                filter_ (\e -> _elementCharacter e ==. val_ (CharacterId (_charId character))) $
                all_ (_tableElements characterDB)
           characters <- all_ (_tableCharacters characterDB)
           guard_ (_elementElement elements `references_` characters)
           return (elements, characters)

-- | Selects all the characters that a user has learned (created a story for)
selectLearnedCharacters :: Connection -> Int -> IO [(Character, UserCharacter)]
selectLearnedCharacters conn userId = undefined

-- | Selects all the characters that a user has not learned
selectNotlearnedCharacters :: Connection -> Int -> IO [Character]
selectNotlearnedCharacters conn userId = undefined

-- | Marks a character learned with a story
insertLearnedCharacter :: Connection -> Int -> Int -> Text -> IO ()
insertLearnedCharacter conn userId characterId story =
    runBeamPostgres conn $ runInsert $
        insert 
            (_tableUserCharacters characterDB)
            ( insertExpressions
                [ UserCharacter
                    default_
                    (UserId (val_ userId))
                    (CharacterId (val_ characterId))
                    (val_ story)
                ]
            )

-- | Selects the user for logging in (apply encryption etc with salt)
selectUserForLogin :: Connection -> Text -> Text -> IO (Maybe User)
selectUserForLogin conn username password = do
    user <- runBeamPostgres conn $ 
                runSelectReturningOne $ select $
                filter_ (\u -> _userUsername u ==. val_ username) $
                all_ (_tableUsers characterDB)
    return (user >>= checkPassword password)

checkPassword :: Text -> User -> Maybe User
checkPassword password user =
    let passH = generate (prfHMAC SHA256) (Parameters 4000 32) (encodeUtf8 password) (encodeUtf8 $ _userSalt user) :: ByteString
    in if decodeUtf8 (convertToBase Base64 passH) == _userPasswordHash user
            then Just user
            else Nothing 

-- | Simple helper function to extract values required for UserJWT
jwtFromUser :: User -> (Int, Text)
jwtFromUser user = (_userId user, _userUsername user)

userNameFromUser :: User -> Text
userNameFromUser = _userUsername

-- | Inserts a new user into the database
registerNewUser :: Connection -> Text -> Text -> Text -> IO User
registerNewUser conn username email pwd = do
    gen         <- getSystemDRG
    let (bs, _) = randomBytesGenerate 32 gen :: (ByteString, SystemDRG)
        salt    = convertToBase Base64 bs
        passH   = generate (prfHMAC SHA256) (Parameters 4000 32) (encodeUtf8 pwd) salt :: ByteString
        passH'  = decodeUtf8 (convertToBase Base64 passH)
    users      <- catchViolation catchUniqueViolation $
                runBeamPostgres conn $ runInsertReturningList $
                insert (_tableUsers characterDB)
                    ( insertExpressions
                        [ User 
                            default_ 
                            (val_ username) 
                            (val_ email) 
                            (val_ passH') 
                            (val_ $ decodeUtf8 salt)
                        ]
                    )
    return (head users)

newtype ViolationError = ViolationError Text deriving Show

instance Exception ViolationError

catchUniqueViolation :: SqlError -> ConstraintViolation -> IO [User]
catchUniqueViolation _ (UniqueViolation "users_username_key") = throwIO (ViolationError "Username is already in use.")
catchUniqueViolation _ (UniqueViolation "users_email_key")    = throwIO (ViolationError "E-Mail is already in use.")
catchUniqueViolation e _                                      = throwIO e
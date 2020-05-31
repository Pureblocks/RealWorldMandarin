{-# LANGUAGE UndecidableInstances #-}

module Database.WordADayDB
    ( insertWords
    , selectAllWords
    , selectAllTweets
    , selectRandom
    , insertTweet
    , selectLatestTweet
    ) where

import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Backend.SQL.BeamExtensions
import Data.Text (Text)
import qualified Data.Text as T
import Data.List.Split (chunksOf)
import Database.Beam.Backend.SQL
import Data.Time
import Words.MandarinHSKWord (HskLevel(..), MandarinHSKWord(..))
import Twitter.TwitterClient (HskTweet(..), HskTweetCreated(..))
import System.Random (randomRIO)
import Data.Maybe (fromJust, maybe)
import Database.Beam.Extended

data WordADayDB f = WordADayDB
                    { _tableHskWords :: f (TableEntity MandarinWordT) 
                    , _tableTweets :: f (TableEntity TweetT) 
                    } deriving (Generic, Database be)

wordADayDB :: DatabaseSettings be WordADayDB
wordADayDB = defaultDbSettings

data MandarinWordT f 
    = MandarinWordT 
    { _wordId           :: Columnar f Int
    , _wordLevel        :: Columnar f HskLevel
    , _wordSimplified   :: Columnar f Text
    , _wordTraditional  :: Columnar f Text
    , _wordPinyinNumber :: Columnar f Text
    , _wordPinyinTones  :: Columnar f Text
    , _wordDefinition   :: Columnar f Text
    } deriving (Generic, Beamable)

type MandarinWord = MandarinWordT Identity
type MandarinWordId = PrimaryKey MandarinWordT Identity

deriving instance Show MandarinWord
deriving instance Eq MandarinWord

instance Table MandarinWordT where
   data PrimaryKey MandarinWordT f = MandarinWordId (Columnar f Int) deriving (Generic, Beamable)
   primaryKey = MandarinWordId . _wordId

deriving instance Show (PrimaryKey MandarinWordT Identity)
deriving instance Eq (PrimaryKey MandarinWordT Identity)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be HskLevel where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => HasSqlEqualityCheck be HskLevel

instance FromBackendRow Postgres HskLevel where
  fromBackendRow = read . T.unpack <$> fromBackendRow

data TweetT f
  = TweetT
  { _tweetId         :: Columnar f Int
  , _tweetLevel      :: Columnar f HskLevel
  , _tweetCreateDate :: Columnar f UTCTime
  , _tweetContent    :: Columnar f Text
  } deriving (Generic, Beamable)

type Tweet = TweetT Identity
deriving instance Show Tweet
deriving instance Eq Tweet

instance Table TweetT where
   data PrimaryKey TweetT f = TweetId (Columnar f Int) deriving (Generic, Beamable)
   primaryKey = TweetId . _tweetId

insertWords :: Connection -> [MandarinHSKWord] -> IO ()
insertWords conn = mapM_ (runBeamPostgres conn . 
    runInsert .
    insert (_tableHskWords wordADayDB) .
    insertValues) . 
    chunksOf 50 .
    fmap toMandarinWord

toMandarinWord :: MandarinHSKWord -> MandarinWord
toMandarinWord mh 
  = MandarinWordT
    (wordId mh)
    (hskLevel mh)
    (simplified mh)
    (traditional mh)
    (pinyinNumeric mh)
    (pinyinTones mh)
    (definition mh)

selectAllWords :: Connection -> IO [MandarinHSKWord]
selectAllWords conn = do
    mandarinwords <- runBeamPostgres conn $ runSelectReturningList $ select (all_ (_tableHskWords wordADayDB))
    return $ fmap toMandarinHSKWord mandarinwords

toMandarinHSKWord :: MandarinWord -> MandarinHSKWord
toMandarinHSKWord mw 
  = MandarinHSKWord
    (_wordId mw)
    (_wordLevel mw)
    (_wordSimplified mw)
    (_wordTraditional mw)
    (_wordPinyinNumber mw)
    (_wordPinyinTones mw)
    (_wordDefinition mw)

selectRandom :: Connection -> HskLevel -> IO MandarinHSKWord
selectRandom conn level = 
  let minLevelQ = aggregate_ (fromMaybe_ 0 . min_ . _wordId) $ 
                        filter_ (\w -> _wordLevel w ==. val_ level) $ 
                        all_ (_tableHskWords wordADayDB)
      maxLevelQ = aggregate_ (fromMaybe_ 0 . max_ . _wordId) $ 
                        filter_ (\w -> _wordLevel w ==. val_ level) $ 
                        all_ (_tableHskWords wordADayDB)
      wordQuery rId   = filter_ (\w -> _wordId w ==. val_ rId) $ 
                        all_ (_tableHskWords wordADayDB)
  in do
  minLevel     <- runSelectOne conn (select minLevelQ)
  maxLevel     <- runSelectOne conn (select maxLevelQ)
  randomId     <- randomRIO (minLevel, maxLevel)
  mandarinWord <- runSelectOne conn (select (wordQuery randomId))
  return (toMandarinHSKWord mandarinWord)

selectLatestTweet :: Connection -> IO (Maybe HskTweetCreated)
selectLatestTweet conn =
  let maxIdQ = aggregate_ (fromMaybe_ 1 . max_ . _tweetId) $ 
                        all_ (_tableTweets wordADayDB)
      tweetQ tId = filter_ (\t-> _tweetId t ==. val_ tId) $ 
                        all_ (_tableTweets wordADayDB)
      selectTweet = 
        runBeamPostgres conn . 
        fmap (fmap toHskTweetCreated) . 
        runSelectReturningOne . select . tweetQ
  in do
    maxId <- runBeamPostgres conn (runSelectReturningOne $ select maxIdQ)
    maybe (pure Nothing) selectTweet maxId

insertTweet :: Connection -> HskTweet -> IO Int
insertTweet conn ht = fmap (_tweetId . head) $
  runBeamPostgres conn $
  runInsertReturningList $
  insert (_tableTweets wordADayDB) $
  insertExpressions (pure $ toTweet ht)

toTweet :: HskTweet -> TweetT (QExpr Postgres s)
toTweet ht = TweetT 
  default_
  (val_ $ tweetLevel ht)
  (val_ $ tweetPlaceDate ht)
  (val_ $ tweetContent ht)

selectAllTweets :: Connection -> IO [HskTweetCreated]
selectAllTweets conn = do
   tweets <- runBeamPostgres conn $ runSelectReturningList $ select (all_ (_tableTweets wordADayDB))
   return $ fmap toHskTweetCreated tweets

toHskTweetCreated :: Tweet -> HskTweetCreated
toHskTweetCreated tweet 
  = HskTweetCreated 
    (_tweetId tweet)
    (_tweetLevel tweet)
    (_tweetCreateDate tweet)
    (_tweetContent tweet)
    
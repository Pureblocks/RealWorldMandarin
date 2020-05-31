module Scheduling.Scheduler
    ( scheduleTweets
    ) where

import Database.Beam.Postgres (Connection)
import Control.Concurrent (threadDelay)
import Database.WordADayDB (selectRandom, insertWords, insertTweet, selectLatestTweet)
import Words.MandarinHSKWord (HskLevel(..), MandarinHSKWord(..))
import Twitter.TwitterClient
import Importer.WordImporter (importLevels)
import Data.Time.Clock
import Configuration.Config (Config(..))
import Data.Maybe (maybe, fromJust)
import Control.Exception (catch)
import Network.HTTP.Client (HttpException)

scheduleTweets :: Config -> Connection -> IO ()
scheduleTweets config conn = do
    _           <- putStrLn "Performing scheduler action!"
    latestTweet <- selectLatestTweet conn
    currentTime <- getCurrentTime
    _ <- if dayPastSinceLastTweet currentTime latestTweet
            then catch (placeNewTweets config conn) (\e -> print (e :: HttpException)) 
            else pure ()
    threadDelay 10000000
    scheduleTweets config conn

dayPastSinceLastTweet :: UTCTime -> Maybe HskTweetCreated -> Bool
dayPastSinceLastTweet ct = maybe True (\tweet -> 
    ct > addUTCTime nominalDay (placeDate tweet))

createAndInsertTweet :: Connection -> MandarinHSKWord -> IO HskTweetCreated
createAndInsertTweet conn ht = do
    tweet   <- createTweet ht
    tweetId <- insertTweet conn tweet
    return $ toHskTweetCreated tweet tweetId

-- TODO rollback the created tweets when the postTweet on twitter goes wrong
placeNewTweets :: Config -> Connection -> IO ()
placeNewTweets config conn = do
    words    <- mapM (selectRandom conn) [minBound..maxBound]
    let twitterConf = twitterConfig config
    tweets   <- mapM (createAndInsertTweet conn) words
    mapM_ (postTweet twitterConf . content) tweets
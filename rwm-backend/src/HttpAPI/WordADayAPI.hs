{-# LANGUAGE DataKinds, TypeOperators #-}

module HttpAPI.WordADayAPI
    ( WordADayAPI(..)
    , wordADayServer
    ) where

import Servant
import Control.Monad.Except
import Database.WordADayDB (selectAllTweets, selectAllWords)
import Database.Beam.Postgres (Connection)
import Words.MandarinHSKWord (MandarinHSKWord)
import Twitter.TwitterClient (HskTweetCreated)
import GHC.Generics (Generic)
import Data.Text
import Data.Aeson

type WordADayAPI = "api" :> "word-a-day" :> "tweets"     :> Get '[JSON] [HskTweetCreated]
              :<|> "api" :> "word-a-day" :> "hsk-words"  :> Get '[JSON] [MandarinHSKWord] 

wordADayServer :: Connection -> Server WordADayAPI
wordADayServer conn = liftIO (selectAllTweets conn) 
                 :<|> liftIO (selectAllWords conn)
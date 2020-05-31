{-# LANGUAGE DataKinds, TypeOperators #-}

module HttpAPI.API
    ( app
    ) where

import Servant
import Control.Monad.Except
import Control.Monad.Reader
import Database.WordADayDB (selectAllTweets, selectAllWords)
import Database.Beam.Postgres (Connection)
import Words.MandarinHSKWord (MandarinHSKWord)
import Twitter.TwitterClient (HskTweetCreated)

type API = "api" :> "tweets" :> Get '[JSON] [HskTweetCreated]
      :<|> "api" :> "hsk-words"  :> Get '[JSON] [MandarinHSKWord] 

server :: Connection -> Server API
server conn = liftIO (selectAllTweets conn) 
         :<|> liftIO (selectAllWords conn)

api :: Proxy API
api = Proxy

app :: Connection -> Application
app = serve api . server
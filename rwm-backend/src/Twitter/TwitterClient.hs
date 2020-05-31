module Twitter.TwitterClient
    ( HskTweet(..)
    , HskTweetCreated(..)
    , postTweet
    , createTweet
    , toHskTweetCreated
    ) where

import qualified Data.Text as T
import Data.Text (Text, append, singleton)
import Data.Time
import Data.Aeson (ToJSON)
import Words.MandarinHSKWord (MandarinHSKWord(..), HskLevel(..))
import GHC.Generics (Generic)
import Data.Time.Clock
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.Wreq
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import Crypto.Hash (Digest)
import Crypto.Hash.Algorithms (SHA1)
import Crypto.Random
import Configuration.Config (TwitterConfig(..))
import Network.URI.Encode (encodeText)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.ByteArray.Encoding (convertToBase, Base(Base64))
import Data.ByteString (ByteString, unpack, empty)
import Data.Char (isAlphaNum)
import Control.Lens ((&), (.~))
import Data.Bifunctor (bimap)

data HskTweet 
    = HskTweet 
    { tweetLevel     :: !HskLevel
    , tweetPlaceDate :: !UTCTime
    , tweetContent   :: !Text
    } deriving (Show, Eq, Generic)

instance ToJSON HskTweet

toHskTweetCreated :: HskTweet -> Int -> HskTweetCreated
toHskTweetCreated ht tweetId
    = HskTweetCreated
    tweetId
    (tweetLevel ht)
    (tweetPlaceDate ht)
    (tweetContent ht)

data HskTweetCreated
    = HskTweetCreated 
    { tweetId   :: !Int
    , level     :: !HskLevel
    , placeDate :: !UTCTime
    , content   :: !Text
    } deriving (Show, Eq, Generic)

instance ToJSON HskTweetCreated

createTweet :: MandarinHSKWord -> IO HskTweet
createTweet mw = do
    time <- getCurrentTime
    let content = "The #Mandarin word of today from #HSK level " `append`
                    getHSKLevelNumber (hskLevel mw) `append`
                    " is " `append` simplified mw `append` 
                    " / " `append` traditional mw `append`
                    " (Simplified / Traditional) " `append`
                    pinyinNumeric mw `append` " / " `append`
                    pinyinTones mw `append` " which means: " `append`
                    definition mw `append` ". #MandarinWordADay #RealWorldMandarin"
    return $ HskTweet (hskLevel mw) time content

getHSKLevelNumber :: HskLevel -> Text
getHSKLevelNumber = singleton . last . show

twitterTweetURL :: Text 
twitterTweetURL = "https://api.twitter.com/1.1/statuses/update.json"

collectParameters :: TwitterConfig -> Text -> Text -> Text -> Text
collectParameters config status timestamp nounce =
    let parameters = [ ("oauth_consumer_key", apiKey config)
                     , ("oauth_nonce", nounce)
                     , ("oauth_signature_method", "HMAC-SHA1")
                     , ("oauth_timestamp", timestamp)
                     , ("oauth_token", accessToken config)
                     , ("oauth_version", "1.0")
                     , ("status", status)
                     ]
        encodedParameters = fmap (bimap encodeText encodeText) parameters  
    in collect "" encodedParameters


collect :: Text -> [(Text, Text)] -> Text
collect acc [(k, v)]   = acc `append` k `append` "=" `append` v
collect acc ((k, v):t) = collect (acc `append` k `append` "=" `append` v `append` "&") t 

signatureBaseString :: Text -> Text
signatureBaseString parameterString = "POST&" `append`
    encodeText twitterTweetURL `append` "&" `append`
    encodeText parameterString

signingKey ::TwitterConfig -> Text
signingKey config = encodeText (apiSecretKey config) `append`
    "&" `append` encodeText (accessTokenSecret config)

calculateSignature :: Text -> Text -> Text
calculateSignature sk = decodeUtf8 . convertToBase Base64 . hmac' sk

hmac' :: Text -> Text -> Digest SHA1 
hmac' sk = hmacGetDigest . hmac (encodeUtf8 sk) . encodeUtf8

getTimeStamp :: IO Text
getTimeStamp = T.pack . show . round <$> getPOSIXTime

getNonce :: IO Text
getNonce = do
    gen <- getSystemDRG
    let (bs, _) = randomBytesGenerate 32 gen :: (ByteString, SystemDRG)
    return $ T.filter isAlphaNum (decodeUtf8 $ convertToBase Base64 bs)

oAuthSignature :: TwitterConfig -> Text -> Text -> Text -> Text
oAuthSignature config timestamp nonce content = 
    let parString  = collectParameters config content timestamp nonce
        sbString   = signatureBaseString parString
        skeyString = signingKey config
    in calculateSignature skeyString sbString

buildHttpOptions :: TwitterConfig -> Text -> Text -> Text -> Text -> Options
buildHttpOptions config content authSignature nonce timestamp =
    let params      = param "status" .~ [content]
        oAuthValues = [ encodeUtf8 $ "OAuth "
                      `append` "oauth_consumer_key=\"" `append` encodeText (apiKey config) `append` "\","
                      `append` "oauth_nonce=\""        `append` encodeText nonce `append` "\","
                      `append` "oauth_signature=\""    `append` encodeText authSignature `append` "\","
                      `append` "oauth_signature_method=\"HMAC-SHA1\","
                      `append` "oauth_timestamp=" `append` "\"" `append` encodeText timestamp `append` "\","
                      `append` "oauth_token=\"" `append` encodeText (accessToken config) `append` "\","
                      `append` "oauth_version=\"1.0\""
                      ]
        headers     = header "Authorization" .~ oAuthValues
    in defaults & params & headers & header "Content-Type" .~ ["application/json"]

postTweet :: TwitterConfig -> Text -> IO ()
postTweet config content = do 
    timestamp         <- getTimeStamp
    nonce             <- getNonce
    let authSignature = oAuthSignature config timestamp nonce content
        options       = buildHttpOptions config content authSignature nonce timestamp
        url           = T.unpack twitterTweetURL
    result <- postWith options url empty
    print result 

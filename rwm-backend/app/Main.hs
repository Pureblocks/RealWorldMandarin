module Main where

import Database.Beam.Postgres (Connection, ConnectInfo(..), connect)
import Scheduling.Scheduler (scheduleTweets)
import Control.Concurrent (forkIO)
import HttpAPI.API (app)
import Network.Wai.Handler.Warp
import Configuration.Config
import Importer.WordImporter (importLevels)
import Database.WordADayDB (insertWords)
import qualified Data.Text as T
import Data.Word (Word16)
import Importer.CharacterImporter (importCharacters)
import Database.CharacterDB (insertCharacters)

main :: IO ()
main = do
    config   <- readConfig
    conn     <- connectToPostgres (postgresConfig config)
    -- immportHskWords conn
    -- importAndSafeCharacters conn
    threadId <- forkIO (scheduleTweets config conn)
    _        <- putStrLn ("Scheduler started on threadId: " ++ show threadId)
    _        <- putStrLn "Starting API"
    run 8080 (app conn)

connectToPostgres :: PostgresConfig -> IO Connection
connectToPostgres config =
    connect (ConnectInfo 
        (T.unpack (host config))
        (fromInteger (port config))
        (T.unpack (user config))
        (T.unpack (password config))
        (T.unpack (database config)))

immportHskWords :: Connection -> IO ()
immportHskWords conn = do
    (n, hskWords) <- importLevels
    _             <- insertWords conn hskWords 
    putStrLn ("Imported and saved a total of " ++ show n ++ " HSK words.")

importAndSafeCharacters :: Connection -> IO ()
importAndSafeCharacters conn = do
    characters <- importCharacters
    _          <- insertCharacters conn characters
    putStrLn ("Imported and saved a total of " ++ show (length characters) ++ " characters.")
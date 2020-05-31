module Importer.CharacterImporter
    ( importCharacters
    , CharacterImport(..)
    ) where

import Data.Text (Text)
import qualified Data.Text as T

data CharacterImport =
    CharacterImport
        { charId         :: !Int
        , hanziCharacter :: !Text
        , keyword        :: !Text
        , primitive      :: !Bool
        , elements       :: ![Int]
        } deriving Show

importCharacters :: IO [CharacterImport]
importCharacters = do
    characterFile     <- readFile "resources/characters.csv"
    let characterLines = tail (T.lines (T.pack characterFile))
    return (fmap readCharacterFromLine characterLines)
    

readCharacterFromLine :: Text -> CharacterImport
readCharacterFromLine t = 
    let list = T.splitOn "," t
        i    = read (T.unpack (head list))
        hc   = list !! 1
        k    = list !! 2
        p    = read (T.unpack (list !! 3))
        e    = list !! 4
        e'   = 
            if e == "NULL"
                then []
                else fmap (read . T.unpack) (T.splitOn ";" e)
    in CharacterImport i hc k p e'

module Importer.WordImporter 
    ( importLevels
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Words.MandarinHSKWord (MandarinHSKWord(..), HskLevel(..))

mandarinWordFromLine :: Int -> HskLevel -> Text -> MandarinHSKWord
mandarinWordFromLine index level line = 
    let entries = T.splitOn "\t" (T.dropWhileEnd (== '\r') line)
        simpl = head entries
        trad  = entries !! 1
        num   = entries !! 2
        tone  = entries !! 3
        def   = entries !! 4
    in MandarinHSKWord index level simpl trad num tone def

importLevel :: Int -> HskLevel -> IO (Int, [MandarinHSKWord])
importLevel fromIndex level = do
    file        <- readFile ("resources/" ++ show level ++ ".txt")
    let entries = T.lines (T.pack file)
    return (length entries, fmap (\(i, e) -> mandarinWordFromLine i level e) (zip [fromIndex..] entries))

importLevels :: IO (Int, [MandarinHSKWord])
importLevels = do
    (n1, hsk1) <- importLevel 1 HSK1
    (n2, hsk2) <- importLevel (n1 + 1) HSK2
    (n3, hsk3) <- importLevel (n1 + n2 + 1) HSK3
    (n4, hsk4) <- importLevel (n1 + n2 + n3 + 1) HSK4
    (n5, hsk5) <- importLevel (n1 + n2 + n3 + n4 + 1) HSK5
    (n6, hsk6) <- importLevel (n1 + n2 + n3 + n4 + n5 + 1) HSK6
    return (n1 + n2 + n3 + n4 + n5 + n6, hsk1 ++ hsk2 ++ hsk3 ++ hsk4 ++ hsk5 ++ hsk6)

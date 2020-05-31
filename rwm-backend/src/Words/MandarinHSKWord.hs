module Words.MandarinHSKWord
    ( MandarinHSKWord(..)
    , HskLevel(..)
    ) where

import Data.Aeson
import GHC.Generics (Generic)
import Data.Text (Text)

data MandarinHSKWord
    = MandarinHSKWord
    { wordId        :: Int
    , hskLevel      :: HskLevel
    , simplified    :: Text
    , traditional   :: Text
    , pinyinNumeric :: Text
    , pinyinTones   :: Text
    , definition    :: Text
    } deriving (Show, Eq, Generic)

instance ToJSON MandarinHSKWord

data HskLevel = HSK1
              | HSK2
              | HSK3
              | HSK4
              | HSK5
              | HSK6
              deriving (Show, Read, Eq, Ord, Enum, Generic, Bounded)

instance ToJSON HskLevel
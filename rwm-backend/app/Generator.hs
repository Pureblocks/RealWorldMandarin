{-# LANGUAGE OverloadedStrings #-}

module Generator where

import Data.Foldable
import qualified Data.HashMap.Lazy as HashMap
import Data.Text (Text, unpack, splitOn, intercalate)
import GHC.Generics
import qualified Language.Elm.Pretty as Pretty
import qualified Language.Elm.Simplification as Simplification
import Language.Haskell.To.Elm
import Servant.To.Elm
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix (takeDirectory)
import Generators.AuthGenerators (authDefinitions)
import Generators.CharacterGenerators (characterDefinitions)

main :: IO ()
main = do 
    putStrLn "Started: generating clients."
    let definitions = authDefinitions ++ characterDefinitions
        modules =
            Pretty.modules $
                Simplification.simplifyDefinition <$> definitions

    forM_ (HashMap.toList modules) $ \(_moduleName, contents) ->
        createAndWriteFile ("../rwm-frontend/src" ++ foldMap ((++) "/" . unpack . intercalate "/" . splitOn ".") _moduleName ++ ".elm") (show contents)
    putStrLn "Finished: generating clients."

createAndWriteFile :: FilePath -> String -> IO ()
createAndWriteFile path content = do
  createDirectoryIfMissing True $ takeDirectory path
  writeFile path content

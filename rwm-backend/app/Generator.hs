{-# language DataKinds #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}

module Generator where

import HttpAPI.AuthAPI (AuthAPI, Register, Login, UserLogin)
import qualified Data.Aeson as Aeson
import Data.Foldable
import qualified Data.HashMap.Lazy as HashMap
import Data.Text (Text, unpack, splitOn, intercalate)
import qualified Generics.SOP as SOP
import GHC.Generics
import Servant.API
import qualified Language.Elm.Pretty as Pretty
import qualified Language.Elm.Simplification as Simplification
import Language.Haskell.To.Elm
import Servant.To.Elm
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix (takeDirectory)

main :: IO ()
main = do 
    putStrLn "Started: generating clients."
    let definitions = map (elmEndpointDefinition "Config.urlBase" ["Clients.AuthAPI"]) (elmEndpoints @AuthAPI)
            <> jsonDefinitions @Register 
            <> jsonDefinitions @Login
            <> jsonDefinitions @UserLogin
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

instance SOP.Generic Register
instance SOP.HasDatatypeInfo Register

instance HasElmType Register where
    elmDefinition =
        Just $ deriveElmTypeDefinition @Register defaultOptions "Clients.Models.AuthAPI.Register"

instance HasElmDecoder Aeson.Value Register where
  elmDecoderDefinition =
    Just $ deriveElmJSONDecoder @Register defaultOptions Aeson.defaultOptions "Clients.Models.AuthAPI.registerDecoder"

instance HasElmEncoder Aeson.Value Register where
  elmEncoderDefinition =
    Just $ deriveElmJSONEncoder @Register defaultOptions Aeson.defaultOptions "Clients.Models.AuthAPI.registerEncoder"

instance SOP.Generic Login
instance SOP.HasDatatypeInfo Login

instance HasElmType Login where
    elmDefinition =
        Just $ deriveElmTypeDefinition @Login defaultOptions "Clients.Models.AuthAPI.Login"

instance HasElmDecoder Aeson.Value Login where
  elmDecoderDefinition =
    Just $ deriveElmJSONDecoder @Login defaultOptions Aeson.defaultOptions "Clients.Models.AuthAPI.loginDecoder"

instance HasElmEncoder Aeson.Value Login where
  elmEncoderDefinition =
    Just $ deriveElmJSONEncoder @Login defaultOptions Aeson.defaultOptions "Clients.Models.AuthAPI.loginEncoder"

instance SOP.Generic UserLogin
instance SOP.HasDatatypeInfo UserLogin

instance HasElmType UserLogin where
    elmDefinition =
        Just $ deriveElmTypeDefinition @UserLogin defaultOptions "Clients.Models.AuthAPI.UserLogin"

instance HasElmDecoder Aeson.Value UserLogin where
  elmDecoderDefinition =
    Just $ deriveElmJSONDecoder @UserLogin defaultOptions Aeson.defaultOptions "Clients.Models.AuthAPI.userLoginDecoder"

instance HasElmEncoder Aeson.Value UserLogin where
  elmEncoderDefinition =
    Just $ deriveElmJSONEncoder @UserLogin defaultOptions Aeson.defaultOptions "Clients.Models.AuthAPI.userLoginEncoder"



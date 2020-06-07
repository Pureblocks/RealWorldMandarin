{-# language DataKinds #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}
{-# language AllowAmbiguousTypes #-}
{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language DuplicateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings #-}
{-# language PolyKinds #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Generator where

import HttpAPI.AuthAPI (AuthAPI, Register, Login, UserJWT)
import qualified Data.Aeson as Aeson
import Data.Foldable
import qualified Data.HashMap.Lazy as HashMap
import Data.Text (Text, unpack, splitOn, intercalate)
import qualified Generics.SOP as SOP
import GHC.Generics
import Servant.API
import Servant.Auth.Server
import qualified Language.Elm.Pretty as Pretty
import qualified Language.Elm.Simplification as Simplification
import Language.Haskell.To.Elm
import Servant.To.Elm
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix (takeDirectory)
import HttpAPI.FrontEnd (ElmSeed)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

main :: IO ()
main = do 
    putStrLn "Started: generating clients."
    let definitions = map (elmEndpointDefinition "Config.urlBase" ["Clients.AuthAPI"]) (elmEndpoints @AuthAPI)
            <> jsonDefinitions @Register 
            <> jsonDefinitions @Login
            <> jsonDefinitions @ElmSeed
            <> jsonDefinitions @UserJWT
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

instance SOP.Generic ElmSeed
instance SOP.HasDatatypeInfo ElmSeed

instance HasElmType ElmSeed where
    elmDefinition =
        Just $ deriveElmTypeDefinition @ElmSeed defaultOptions "Models.ElmSeed.ElmSeed"

instance HasElmDecoder Aeson.Value ElmSeed where
  elmDecoderDefinition =
    Just $ deriveElmJSONDecoder @ElmSeed defaultOptions Aeson.defaultOptions "Models.ElmSeed.elmSeedDecoder"

instance HasElmEncoder Aeson.Value ElmSeed where
  elmEncoderDefinition =
    Just $ deriveElmJSONEncoder @ElmSeed defaultOptions Aeson.defaultOptions "Models.ElmSeed.elmSeedEncoder"

instance SOP.Generic UserJWT
instance SOP.HasDatatypeInfo UserJWT

instance HasElmType UserJWT where
    elmDefinition =
        Just $ deriveElmTypeDefinition @UserJWT defaultOptions "Clients.Models.AuthAPI.UserJWT"

instance HasElmDecoder Aeson.Value UserJWT where
  elmDecoderDefinition =
    Just $ deriveElmJSONDecoder @UserJWT defaultOptions Aeson.defaultOptions "Clients.Models.AuthAPI.userJwtDecoder"

instance HasElmEncoder Aeson.Value UserJWT where
  elmEncoderDefinition =
    Just $ deriveElmJSONEncoder @UserJWT defaultOptions Aeson.defaultOptions "Clients.Models.AuthAPI.userJwtEncoder"

-- | TODO extract this to special generator for API endpoints since this is very dangerous
-- creates an UserJWT encoder and decoder for each POST call that returns a 201'
instance HasElmEndpoints (Verb 'POST 201 list a) where
    elmEndpoints' prefix =
      [ prefix
        { _method = "POST"
        , _returnType =  Just $ Right $ makeDecoder @Aeson.Value @UserJWT
        , _functionName = Text.toLower (Text.decodeUtf8 "POST") : _functionName prefix
        }
      ]
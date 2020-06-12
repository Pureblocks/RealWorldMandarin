{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings #-}
{-# language PolyKinds #-}
{-# language TypeApplications #-}

module Generators.AuthGenerators
    ( authDefinitions
    ) where

import HttpAPI.AuthAPI (AuthAPI, Register, Login, UserJWT)
import qualified Data.Aeson as Aeson
import qualified Generics.SOP as SOP
import GHC.Generics
import Servant.API
import Language.Haskell.To.Elm
import Servant.To.Elm
import HttpAPI.FrontEnd (ElmSeed)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Language.Elm.Definition (Definition)

authDefinitions :: [Definition]
authDefinitions = 
    map (elmEndpointDefinition "Config.urlBase" ["Clients.AuthAPI"]) (elmEndpoints @AuthAPI)
        <> jsonDefinitions @Register 
        <> jsonDefinitions @Login
        <> jsonDefinitions @ElmSeed
        <> jsonDefinitions @UserJWT

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

-- | Dangerous hack for creating a UserJWT response in Elm.
instance HasElmEndpoints (Verb 'POST 201 list a) where
    elmEndpoints' prefix =
      [ prefix
        { _method = "POST"
        , _returnType =  Just $ Right $ makeDecoder @Aeson.Value @UserJWT
        , _functionName = Text.toLower (Text.decodeUtf8 "POST") : _functionName prefix
        }
      ]
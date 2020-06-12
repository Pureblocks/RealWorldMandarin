{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings #-}
{-# language PolyKinds #-}
{-# language TypeApplications #-}

module Generators.CharacterGenerators
    ( characterDefinitions
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
import HttpAPI.CharacterAPI

characterDefinitions :: [Definition]
characterDefinitions =
    map (elmEndpointDefinition "Config.urlBase" ["Clients.CharacterAPI"]) (elmEndpoints @CharacterAPI)
        <> jsonDefinitions @Character 
        <> jsonDefinitions @Element
        <> jsonDefinitions @Story

instance SOP.Generic Character
instance SOP.HasDatatypeInfo Character

instance HasElmType Character where
    elmDefinition =
        Just $ deriveElmTypeDefinition @Character defaultOptions "Clients.Models.CharacterAPI.Character"

instance HasElmDecoder Aeson.Value Character where
  elmDecoderDefinition =
    Just $ deriveElmJSONDecoder @Character defaultOptions Aeson.defaultOptions "Clients.Models.CharacterAPI.characterDecoder"

instance HasElmEncoder Aeson.Value Character where
  elmEncoderDefinition =
    Just $ deriveElmJSONEncoder @Character defaultOptions Aeson.defaultOptions "Clients.Models.CharacterAPI.characterEncoder"

instance SOP.Generic Element
instance SOP.HasDatatypeInfo Element

instance HasElmType Element where
    elmDefinition =
        Just $ deriveElmTypeDefinition @Element defaultOptions "Clients.Models.CharacterAPI.Element"

instance HasElmDecoder Aeson.Value Element where
  elmDecoderDefinition =
    Just $ deriveElmJSONDecoder @Element defaultOptions Aeson.defaultOptions "Clients.Models.CharacterAPI.elementDecoder"

instance HasElmEncoder Aeson.Value Element where
  elmEncoderDefinition =
    Just $ deriveElmJSONEncoder @Element defaultOptions Aeson.defaultOptions "Clients.Models.CharacterAPI.elementEncoder"

instance SOP.Generic Story
instance SOP.HasDatatypeInfo Story

instance HasElmType Story where
    elmDefinition =
        Just $ deriveElmTypeDefinition @Story defaultOptions "Clients.Models.CharacterAPI.Story"

instance HasElmDecoder Aeson.Value Story where
  elmDecoderDefinition =
    Just $ deriveElmJSONDecoder @Story defaultOptions Aeson.defaultOptions "Clients.Models.CharacterAPI.storyDecoder"

instance HasElmEncoder Aeson.Value Story where
  elmEncoderDefinition =
    Just $ deriveElmJSONEncoder @Story defaultOptions Aeson.defaultOptions "Clients.Models.CharacterAPI.storyEncoder"

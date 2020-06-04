module Clients.AuthAPI exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set
import Http
import String
import Url.Builder

maybeBoolToIntStr : Maybe Bool -> String
maybeBoolToIntStr mx =
  case mx of
    Nothing -> ""
    Just True -> "1"
    Just False -> "0"

type alias Register  =
   { registerUsername: String
   , email: String
   , passwordOne: String
   , passwordTwo: String
   }

jsonDecRegister : Json.Decode.Decoder ( Register )
jsonDecRegister =
   Json.Decode.succeed (\pregisterUsername pemail ppasswordOne ppasswordTwo -> {registerUsername = pregisterUsername, email = pemail, passwordOne = ppasswordOne, passwordTwo = ppasswordTwo})
   |> required "registerUsername" (Json.Decode.string)
   |> required "email" (Json.Decode.string)
   |> required "passwordOne" (Json.Decode.string)
   |> required "passwordTwo" (Json.Decode.string)

jsonEncRegister : Register -> Value
jsonEncRegister  val =
   Json.Encode.object
   [ ("registerUsername", Json.Encode.string val.registerUsername)
   , ("email", Json.Encode.string val.email)
   , ("passwordOne", Json.Encode.string val.passwordOne)
   , ("passwordTwo", Json.Encode.string val.passwordTwo)
   ]



type alias Login  =
   { username: String
   , password: String
   }

jsonDecLogin : Json.Decode.Decoder ( Login )
jsonDecLogin =
   Json.Decode.succeed (\pusername ppassword -> {username = pusername, password = ppassword})
   |> required "username" (Json.Decode.string)
   |> required "password" (Json.Decode.string)

jsonEncLogin : Login -> Value
jsonEncLogin  val =
   Json.Encode.object
   [ ("username", Json.Encode.string val.username)
   , ("password", Json.Encode.string val.password)
   ]



type NoContent  =
    NoContent 

jsonDecNoContent : Json.Decode.Decoder ( NoContent )
jsonDecNoContent = 
    let jsonDecDictNoContent = Dict.fromList [("NoContent", NoContent)]
    in  decodeSumUnaries "NoContent" jsonDecDictNoContent

jsonEncNoContent : NoContent -> Value
jsonEncNoContent  val =
    case val of
        NoContent -> Json.Encode.string "NoContent"


postApiAuthLogin : String -> Login -> (Result Http.Error  (NoContent)  -> msg) -> Cmd msg
postApiAuthLogin urlBase body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin urlBase
                    [ "api"
                    , "auth"
                    , "login"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncLogin body)
            , expect =
                Http.expectJson toMsg jsonDecNoContent
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postApiAuthRegister : String -> Register -> (Result Http.Error  (NoContent)  -> msg) -> Cmd msg
postApiAuthRegister urlBase body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin urlBase
                    [ "api"
                    , "auth"
                    , "register"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncRegister body)
            , expect =
                Http.expectJson toMsg jsonDecNoContent
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

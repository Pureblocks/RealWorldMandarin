module Clients.Models.AuthAPI exposing (..)

import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


type alias Register  =
    { registerUsername : String
    , email : String
    , passwordOne : String
    , passwordTwo : String }


registerEncoder : Register -> Json.Encode.Value
registerEncoder a =
    Json.Encode.object [ ("registerUsername" , Json.Encode.string a.registerUsername)
    , ("email" , Json.Encode.string a.email)
    , ("passwordOne" , Json.Encode.string a.passwordOne)
    , ("passwordTwo" , Json.Encode.string a.passwordTwo) ]


registerDecoder : Json.Decode.Decoder Register
registerDecoder =
    Json.Decode.succeed Register |>
    Json.Decode.Pipeline.required "registerUsername" Json.Decode.string |>
    Json.Decode.Pipeline.required "email" Json.Decode.string |>
    Json.Decode.Pipeline.required "passwordOne" Json.Decode.string |>
    Json.Decode.Pipeline.required "passwordTwo" Json.Decode.string


type alias Login  =
    { username : String, password : String }


loginEncoder : Login -> Json.Encode.Value
loginEncoder a =
    Json.Encode.object [ ("username" , Json.Encode.string a.username)
    , ("password" , Json.Encode.string a.password) ]


loginDecoder : Json.Decode.Decoder Login
loginDecoder =
    Json.Decode.succeed Login |>
    Json.Decode.Pipeline.required "username" Json.Decode.string |>
    Json.Decode.Pipeline.required "password" Json.Decode.string
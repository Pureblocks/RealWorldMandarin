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
    { loginUsername : String, password : String }


loginEncoder : Login -> Json.Encode.Value
loginEncoder a =
    Json.Encode.object [ ("loginUsername" , Json.Encode.string a.loginUsername)
    , ("password" , Json.Encode.string a.password) ]


loginDecoder : Json.Decode.Decoder Login
loginDecoder =
    Json.Decode.succeed Login |>
    Json.Decode.Pipeline.required "loginUsername" Json.Decode.string |>
    Json.Decode.Pipeline.required "password" Json.Decode.string


type alias UserJWT  =
    { sub : Int, un : String }


userJwtEncoder : UserJWT -> Json.Encode.Value
userJwtEncoder a =
    Json.Encode.object [ ("sub" , Json.Encode.int a.sub)
    , ("un" , Json.Encode.string a.un) ]


userJwtDecoder : Json.Decode.Decoder UserJWT
userJwtDecoder =
    Json.Decode.succeed UserJWT |>
    Json.Decode.Pipeline.required "sub" Json.Decode.int |>
    Json.Decode.Pipeline.required "un" Json.Decode.string
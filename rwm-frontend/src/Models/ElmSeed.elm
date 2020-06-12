module Models.ElmSeed exposing (..)

import Clients.Models.AuthAPI
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import Maybe.Extra


type alias ElmSeed  =
    { page : String, userJWT : Maybe Clients.Models.AuthAPI.UserJWT }


elmSeedEncoder : ElmSeed -> Json.Encode.Value
elmSeedEncoder a =
    Json.Encode.object [ ("page" , Json.Encode.string a.page)
    , ("userJWT" , Maybe.Extra.unwrap Json.Encode.null Clients.Models.AuthAPI.userJwtEncoder a.userJWT) ]


elmSeedDecoder : Json.Decode.Decoder ElmSeed
elmSeedDecoder =
    Json.Decode.succeed ElmSeed |>
    Json.Decode.Pipeline.required "page" Json.Decode.string |>
    Json.Decode.Pipeline.required "userJWT" (Json.Decode.nullable Clients.Models.AuthAPI.userJwtDecoder)
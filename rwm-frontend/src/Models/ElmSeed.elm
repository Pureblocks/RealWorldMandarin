module Models.ElmSeed exposing (..)

import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import Maybe.Extra


type alias ElmSeed  =
    { page : String, user : Maybe String }


elmSeedEncoder : ElmSeed -> Json.Encode.Value
elmSeedEncoder a =
    Json.Encode.object [ ("page" , Json.Encode.string a.page)
    , ("user" , Maybe.Extra.unwrap Json.Encode.null Json.Encode.string a.user) ]


elmSeedDecoder : Json.Decode.Decoder ElmSeed
elmSeedDecoder =
    Json.Decode.succeed ElmSeed |>
    Json.Decode.Pipeline.required "page" Json.Decode.string |>
    Json.Decode.Pipeline.required "user" (Json.Decode.nullable Json.Decode.string)
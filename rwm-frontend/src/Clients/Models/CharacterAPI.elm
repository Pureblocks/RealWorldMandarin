module Clients.Models.CharacterAPI exposing (..)

import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


type alias Character  =
    { characterId : Int
    , hanzi : String
    , keyword : String
    , primitive : Bool
    , elements : List Element }


characterEncoder : Character -> Json.Encode.Value
characterEncoder a =
    Json.Encode.object [ ("characterId" , Json.Encode.int a.characterId)
    , ("hanzi" , Json.Encode.string a.hanzi)
    , ("keyword" , Json.Encode.string a.keyword)
    , ("primitive" , Json.Encode.bool a.primitive)
    , ("elements" , Json.Encode.list elementEncoder a.elements) ]


characterDecoder : Json.Decode.Decoder Character
characterDecoder =
    Json.Decode.succeed Character |>
    Json.Decode.Pipeline.required "characterId" Json.Decode.int |>
    Json.Decode.Pipeline.required "hanzi" Json.Decode.string |>
    Json.Decode.Pipeline.required "keyword" Json.Decode.string |>
    Json.Decode.Pipeline.required "primitive" Json.Decode.bool |>
    Json.Decode.Pipeline.required "elements" (Json.Decode.list elementDecoder)


type alias Element  =
    { elementCharacterId : Int
    , position : Int
    , elemHanzi : String
    , elementKeyword : String }


elementEncoder : Element -> Json.Encode.Value
elementEncoder a =
    Json.Encode.object [ ("elementCharacterId" , Json.Encode.int a.elementCharacterId)
    , ("position" , Json.Encode.int a.position)
    , ("elemHanzi" , Json.Encode.string a.elemHanzi)
    , ("elementKeyword" , Json.Encode.string a.elementKeyword) ]


elementDecoder : Json.Decode.Decoder Element
elementDecoder =
    Json.Decode.succeed Element |>
    Json.Decode.Pipeline.required "elementCharacterId" Json.Decode.int |>
    Json.Decode.Pipeline.required "position" Json.Decode.int |>
    Json.Decode.Pipeline.required "elemHanzi" Json.Decode.string |>
    Json.Decode.Pipeline.required "elementKeyword" Json.Decode.string


type alias Story  =
    { story : String }


storyEncoder : Story -> Json.Encode.Value
storyEncoder a =
    Json.Encode.object [("story" , Json.Encode.string a.story)]


storyDecoder : Json.Decode.Decoder Story
storyDecoder =
    Json.Decode.succeed Story |>
    Json.Decode.Pipeline.required "story" Json.Decode.string
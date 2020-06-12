module Clients.CharacterAPI exposing (..)

import Clients.Models.CharacterAPI
import Config
import Http
import Json.Decode


getApiCharactersUserByUserIdNext : Int -> Cmd (Result (Http.Error , Maybe { metadata : Http.Metadata
    , body : String }) Clients.Models.CharacterAPI.Character)
getApiCharactersUserByUserIdNext a =
    Http.request { method = "GET"
    , headers = []
    , url = String.join "/" [ Config.urlBase
    , "api/characters/user"
    , String.fromInt a
    , "next" ]
    , body = Http.emptyBody
    , expect = Http.expectStringResponse identity (\b -> case b of
        Http.BadUrl_ c ->
            Err (Http.BadUrl c , Nothing)
        
        Http.Timeout_ ->
            Err (Http.Timeout , Nothing)
        
        Http.NetworkError_ ->
            Err (Http.NetworkError , Nothing)
        
        Http.BadStatus_ c d ->
            Err (Http.BadStatus c.statusCode , Just { metadata = c, body = d })
        
        Http.GoodStatus_ c d ->
            Result.mapError (\e -> (Http.BadBody (Json.Decode.errorToString e) , Just { metadata = c
            , body = d })) (Json.Decode.decodeString Clients.Models.CharacterAPI.characterDecoder d))
    , timeout = Nothing
    , tracker = Nothing }


postApiCharactersUsersByUserIdByCharacterId : Int -> Int -> Clients.Models.CharacterAPI.Story -> Cmd (Result (Http.Error , Maybe { metadata : Http.Metadata
    , body : String }) ())
postApiCharactersUsersByUserIdByCharacterId a b c =
    Http.request { method = "POST"
    , headers = []
    , url = String.join "/" [ Config.urlBase
    , "api/characters/users"
    , String.fromInt a
    , String.fromInt b ]
    , body = Http.jsonBody (Clients.Models.CharacterAPI.storyEncoder c)
    , expect = Http.expectStringResponse identity (\d -> case d of
        Http.BadUrl_ e ->
            Err (Http.BadUrl e , Nothing)
        
        Http.Timeout_ ->
            Err (Http.Timeout , Nothing)
        
        Http.NetworkError_ ->
            Err (Http.NetworkError , Nothing)
        
        Http.BadStatus_ e f ->
            Err (Http.BadStatus e.statusCode , Just { metadata = e, body = f })
        
        Http.GoodStatus_ e f ->
            if f == "" then
                Ok ()
            
            else
                Err (Http.BadBody "Expected the response body to be empty" , Just { metadata = e
                , body = f }))
    , timeout = Nothing
    , tracker = Nothing }
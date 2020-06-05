module Clients.AuthAPI exposing (..)

import Clients.Models.AuthAPI
import Config
import Http


postApiAuthLogin : Clients.Models.AuthAPI.Login -> Cmd (Result (Http.Error , Maybe { metadata : Http.Metadata
    , body : String }) ())
postApiAuthLogin a =
    Http.request { method = "POST"
    , headers = []
    , url = Config.urlBase ++ "/api/auth/login"
    , body = Http.jsonBody (Clients.Models.AuthAPI.loginEncoder a)
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
            if d == "" then
                Ok ()
            
            else
                Err (Http.BadBody "Expected the response body to be empty" , Just { metadata = c
                , body = d }))
    , timeout = Nothing
    , tracker = Nothing }


postApiAuthRegister : Clients.Models.AuthAPI.Register -> Cmd (Result (Http.Error , Maybe { metadata : Http.Metadata
    , body : String }) ())
postApiAuthRegister a =
    Http.request { method = "POST"
    , headers = []
    , url = Config.urlBase ++ "/api/auth/register"
    , body = Http.jsonBody (Clients.Models.AuthAPI.registerEncoder a)
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
            if d == "" then
                Ok ()
            
            else
                Err (Http.BadBody "Expected the response body to be empty" , Just { metadata = c
                , body = d }))
    , timeout = Nothing
    , tracker = Nothing }
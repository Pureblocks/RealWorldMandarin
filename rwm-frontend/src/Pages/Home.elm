module Pages.Home exposing (Model, Msg, init, update, view)

import Browser exposing (Document)
import Html exposing (text)
import Auth exposing (Auth)
import Html exposing (..)
import Html.Attributes exposing (..)

type alias Model =
    { auth: Auth }

type Msg = HomeSampleMessage

init : Auth -> ( Model, Cmd Msg)
init auth = ( { auth = auth }, Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    ( model, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "Real World Mandarin - Home"
    , body = [ a
                [ href "/app/login"
                ]
                [ text "Login" ] ]
    }
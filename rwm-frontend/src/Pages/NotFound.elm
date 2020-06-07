module Pages.NotFound exposing (Model, Msg, init, update, view)

import Browser exposing (Document)
import Html exposing (text)
import Auth exposing (Auth)

type alias Model =
    { auth: Auth }

type Msg = NotFoundSampleMessage

init : Auth -> ( Model, Cmd Msg)
init auth = ( { auth = auth }, Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    ( model, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "Real World Mandarin - NotFound"
    , body = [ text "Hello, NotFound" ]
    }
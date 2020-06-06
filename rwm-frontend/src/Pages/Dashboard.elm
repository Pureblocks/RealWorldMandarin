module Pages.Dashboard exposing (Model, Msg, init, update, view)

import Browser exposing (Document)
import Html exposing (text)

type alias Model =
    {}

type Msg = DashboardSampleMessage

init : () -> ( Model, Cmd Msg)
init _ = ( {}, Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    ( model, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "Real World Mandarin - Dashboard"
    , body = [ text "Hello, dashboard" ]
    }



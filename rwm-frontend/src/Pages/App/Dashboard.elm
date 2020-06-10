module Pages.App.Dashboard exposing (Model, Msg, init, update, view)

import Browser exposing (Document)
import Auth exposing (Auth)
import Router exposing (Route(..), AppRoute(..))
import Html as Html
import Html.Attributes as Attr
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid as Grid
import Html.Lazy exposing (lazy)

type alias Model =
    { auth: Auth }

type Msg = DashboardSampleMessage

init : Auth -> ( Model, Cmd Msg)
init auth = ( { auth = auth }, Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    ( model, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "Real World Mandarin - Dashboard"
    , body = 
        [ Html.text "Content dashboard"
        ] 
    }
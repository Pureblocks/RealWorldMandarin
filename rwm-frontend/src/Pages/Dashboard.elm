module Pages.Dashboard exposing (Model, Msg, init, update, view)

import Browser exposing (Document)
import Auth exposing (Auth)
import Pages.Elements.Menu exposing (menu)
import Router exposing (Route(..))
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
        [ lazy createPage model
        ] 
    }

createPage : Model -> Html.Html Msg
createPage model =
    Grid.containerFluid
        [ Attr.style "background-color" "#F6F4FC" ]
        [ Grid.row
            []
            [ menu Dashboard
            , Grid.col
                [ Col.attrs
                    [ Attr.class "text-center"
                    ]
                ]
                [ Html.h1 
                    []
                    [ Html.text "Content dashboard"
                    ]
                ]
            ]
        ]
module Main exposing (main)

import Browser
import Html exposing (div, button, text, input, p, h1, h2, form)

type alias Model =
    { character : String }

type Msg = Learned

main : Program () Model Msg
main =
  Browser.element
    { init = init
    , subscriptions = \_ -> Sub.none
    , update = update
    , view = view
    }

init : () -> ( Model, Cmd Msg )
init _ = ({ character = "" }, Cmd.none)

update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Learned -> (model, Cmd.none)

view : Model -> Html.Html Msg
view model = h1 [] [text "Hello"]


module Pages.App.Learning exposing (Model, Msg, init, update, view)

import Browser exposing (Document)
import Auth exposing (Auth)
import Router exposing (Route(..), AppRoute(..))
import Html as Html
import Html.Attributes as Attr
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid as Grid
import Bootstrap.Form.Textarea as Textarea
import Html.Lazy exposing (lazy)

type alias Model =
    { auth: Auth }

type Msg = LearningSampleMessage

init : Auth -> ( Model, Cmd Msg)
init auth = ( { auth = auth }, Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    ( model, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "Real World Mandarin - Learning"
    , body = 
        [ Grid.row
            []
            [ Grid.col
                [ Col.md12
                , Col.lg2
                ]
                []
            , Grid.col
                [ Col.sm12
                , Col.lg8
                ]
                [ Grid.row
                    []
                    [ Grid.col
                        [ Col.md12
                        , Col.lg5
                        ]
                        [ Grid.row
                            []
                            [ Grid.col
                                [ Col.xs12
                                , Col.attrs
                                    [ Attr.style "background-color" "#FFA987"
                                    , Attr.style "margin" "60px 30px 15px 30px"
                                    , Attr.style "color" "#1E1E24"
                                    , Attr.class "text-center"
                                    , Attr.style "border" "1px solid #1E1E24"
                                    ] 
                                ]
                                [ Html.div
                                    [ Attr.style "font-size" "36px"
                                    ]
                                    [ Html.span [ Attr.style "color" "1E1E24"] [ Html.text "Friend" ]
                                    ]
                                ]
                            , Grid.col
                                [ Col.xs12
                                , Col.attrs
                                    [ Attr.style "background-color" "#E54B4B"
                                    , Attr.style "margin" "15px 30px 15px 30px"
                                    , Attr.style "color" "#1E1E24"
                                    , Attr.class "text-center"
                                    , Attr.style "border" "1px solid #1E1E24"
                                    ] 
                                ]
                                [ Html.div
                                    [ Attr.style "font-size" "247px" ]
                                    [ Html.text "朋"]
                                ]
                            , Grid.col
                                [ Col.xs12
                                , Col.attrs
                                    [ Attr.style "background-color" "#F7EBE8"
                                    , Attr.style "margin" "15px 30px 15px 30px"
                                    , Attr.style "color" "#444140"
                                    , Attr.class "text-center"
                                    , Attr.style "border" "1px solid #1E1E24"
                                    ] 
                                ]
                                [ Html.div
                                    [ Attr.style "font-size" "36px" ]
                                    [ Html.text "moon (月) + moon (月)"]
                                ]
                            ]

                        ]
                    , Grid.col
                        [ Col.md12
                        , Col.lg7
                        ]
                        [ Grid.row
                            []
                            [ Grid.col
                                [ Col.xs12
                                , Col.attrs
                                    [ Attr.style "margin" "60px 30px 15px 30px"
                                    ] 
                                ]
                                [ Textarea.textarea
                                    [ Textarea.id "storyArea"
                                    , Textarea.rows 16
                                    , Textarea.attrs 
                                        [ Attr.placeholder "Enter your story here..."
                                        , Attr.style "background-color" "#F7EBE8"
                                        , Attr.style "color" "#1E1E24"
                                        , Attr.style "font-size" "18px"
                                        , Attr.style "border" "1px solid #1E1E24"
                                        ]
                                    ]
                                ]
                            , Grid.col
                                [ Col.xs12
                                , Col.attrs
                                    [ Attr.style "background-color" "#E54B4B"
                                    , Attr.style "margin" "15px 30px 15px 30px"
                                    , Attr.style "color" "#F7EBE8"
                                    , Attr.class "text-center"
                                    , Attr.style "border" "1px solid #1E1E24"
                                    ] 
                                ]
                                [ Html.div
                                    [ Attr.style "font-size" "36px" ]
                                    [ Html.text "Learned!" ]
                                ]
                            ]
                        ]
                    ]
                ]
            , Grid.col
                [ Col.md12
                , Col.lg2
                ]
                []
            ]
        ] 
    }
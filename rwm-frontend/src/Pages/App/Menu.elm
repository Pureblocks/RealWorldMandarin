module Pages.App.Menu exposing (menu)

import Router exposing (Route(..), AppRoute(..))
import Html as Html
import Html.Attributes as Attr
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import FontAwesome.Icon as Icon
import FontAwesome.Attributes as FAAttr
import FontAwesome.Styles as FAS
import Html.Events exposing (onClick, onMouseOver)

menu : AppRoute -> Grid.Column msg
menu route =
    Grid.col
        [ Col.attrs 
            [ Attr.style "-ms-flex" "0 0 285px"
            , Attr.style "flex" "0 0 285px"
            , Attr.style "padding" "0"
            , Attr.style "background-color" "#FEFEFE"
            , Attr.style "height" "100vh"
            , Attr.style "border-right" "1px solid #EAE9EE"
            ]
        ]
        [ FAS.css -- Move this to the top level page? :) 
        , Html.a
            [ Attr.class "navbar-brand text-center"
            , Attr.style "margin-top" "25px"
            , Attr.style "margin-left" "55px"
            , Attr.style "color" "#131021"
            , Attr.style "font-weight" "bold"
            , Attr.href "/app/dashboard"
            ]
            [ Html.img 
                [ Attr.class "d-inline-block align-top"
                , Attr.src "/img/logo.png"
                , Attr.width 175
                ]
                []
            ]
        , menuItems route
        ]

menuItems : AppRoute -> Html.Html msg
menuItems currentRoute =
    Html.div
        [ Attr.style "margin-top" "100px"
        , Attr.style "font-size" "20px"
        ]
        (List.map 
            (menuItem currentRoute) 
            [ Dashboard
            , Learning
            , Training
            , Settings
            ]
        )

menuItem : AppRoute -> AppRoute -> Html.Html msg
menuItem currentRoute menuRoute =
    let active = currentRoute == menuRoute
        colorStyle =
            if active
                then Attr.style "color" "#E54B4B"
                else Attr.style "color" "#bbb"
    in
    Html.a
        [ Attr.href (Router.toUrlString (App menuRoute))
        , Attr.style "hover" "" 
        ]
        [ Grid.row
            [ Row.attrs
                ([ Attr.style "margin" "50px 0px"
                , Attr.style "cursor" "pointer"
                ] ++ if active 
                        then [ Attr.style "border-right" "1px solid #E54B4B"] 
                        else [])
            ]
            [ Grid.col
                [ Col.attrs 
                    [ colorStyle
                    ]
                ]
                [ Icon.viewStyled 
                    [ FAAttr.lg
                    , Attr.style "margin" "0px 50px"
                    ] 
                    (Router.getIcon (App menuRoute))
                , Html.text (Router.toString (App menuRoute))
                ]
            ]
        ]


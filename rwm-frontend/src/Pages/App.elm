module Pages.App exposing (Model, SubModel(..), Msg(..), init, update, view)

import Browser exposing (Document)
import Auth exposing (Auth)
import Html.Attributes as Attr
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid as Grid
import Pages.App.Dashboard as PageDashboard
import Pages.App.Learning as PageLearning
import Pages.App.Training as PageTraining
import Pages.App.Settings as PageSettings
import Router as Router
import Html.Lazy exposing (lazy)
import Util exposing (updateWith)
import Html as Html
import FontAwesome.Icon as Icon
import FontAwesome.Attributes as FAAttr
import FontAwesome.Styles as FAS
import Html.Events exposing (onMouseOver, onMouseOut)


type alias Model =
    { currentHoover : Maybe Router.AppRoute
    , subModel : SubModel
    }

type SubModel
    = Dashboard PageDashboard.Model
    | Learning PageLearning.Model
    | Training PageTraining.Model
    | Settings PageSettings.Model

type Msg
    = GotDashboardMsg PageDashboard.Msg
    | GotLearningMsg PageLearning.Msg
    | GotTrainingMsg PageTraining.Msg
    | GotSettingsMsg PageSettings.Msg
    | Hoover Router.AppRoute
    | UnHoover

-- todo push a auth model downstream to init where you always know the user is authenticated
init : Auth -> Router.AppRoute -> ( Model, Cmd Msg)
init auth appRoute = 
    case appRoute of
        Router.Dashboard ->
            PageDashboard.init auth
                |> updateWith (\sm -> { currentHoover = Nothing, subModel = Dashboard sm }) GotDashboardMsg 

        Router.Learning ->
            PageLearning.init auth
                |> updateWith (\sm -> { currentHoover = Nothing, subModel = Learning sm }) GotLearningMsg 

        Router.Training ->
            PageTraining.init auth
                |> updateWith (\sm -> { currentHoover = Nothing, subModel = Training sm }) GotTrainingMsg

        Router.Settings ->
            PageSettings.init auth
                |> updateWith (\sm -> { currentHoover = Nothing, subModel = Settings sm }) GotSettingsMsg 


update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case msg of
        Hoover appRoute ->
            ({ model | currentHoover = Just appRoute }, Cmd.none)
        
        UnHoover ->
            ({ model | currentHoover = Nothing }, Cmd.none)

        _ ->
            case (msg, model.subModel) of 
                (GotDashboardMsg subMsg, Dashboard dashboard) ->
                    PageDashboard.update subMsg dashboard
                        |> updateWith (\sm -> { model | subModel = Dashboard sm }) GotDashboardMsg

                (GotLearningMsg subMsg, Learning learning) ->
                    PageLearning.update subMsg learning
                        |> updateWith (\sm -> { model | subModel = Learning sm }) GotLearningMsg

                (GotTrainingMsg subMsg, Training training) ->
                    PageTraining.update subMsg training
                        |> updateWith (\sm -> { model | subModel = Training sm }) GotTrainingMsg

                (GotSettingsMsg subMsg, Settings settings) ->
                    PageSettings.update subMsg settings
                        |> updateWith (\sm -> { model | subModel = Settings sm }) GotSettingsMsg
                
                (_, _) ->
                    Debug.log "Received message for the wrong page in App" ( model, Cmd.none )


view : Model -> Document Msg
view model =
    let 
        viewPage pageView toMsg subModel subRoute =
            let 
                { title, body } =
                    pageView subModel
            in 
            { title = title
            , body = 
                [ Grid.containerFluid
                    [ Attr.style "background-color" "#F6F4FC" ]
                    [ Grid.row
                        []
                        [ menu subRoute model.currentHoover
                        , Grid.col
                            [ Col.attrs
                                []
                            ]
                            (List.map (Html.map toMsg) body)
                        ]
                    ]
                ]
            }
    in
    case model.subModel of
        Dashboard m ->
            viewPage PageDashboard.view GotDashboardMsg m Router.Dashboard

        Learning m ->
            viewPage PageLearning.view GotLearningMsg m Router.Learning

        Training m ->
            viewPage PageTraining.view GotTrainingMsg m Router.Training

        Settings m ->
            viewPage PageSettings.view GotSettingsMsg m Router.Settings

menu : Router.AppRoute -> Maybe Router.AppRoute -> Grid.Column Msg
menu route maybeHoover =
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
        , menuItems route maybeHoover
        ]

menuItems : Router.AppRoute -> Maybe Router.AppRoute -> Html.Html Msg
menuItems currentRoute maybeHoover =
    Html.div
        [ Attr.style "margin-top" "100px"
        , Attr.style "font-size" "20px"
        ]
        (List.map 
            (menuItem currentRoute maybeHoover) 
            [ Router.Dashboard
            , Router.Learning
            , Router.Training
            , Router.Settings
            ]
        )

menuItem : Router.AppRoute -> Maybe Router.AppRoute -> Router.AppRoute -> Html.Html Msg
menuItem currentRoute maybeHoover menuRoute =
    let active = currentRoute == menuRoute || Maybe.withDefault False (Maybe.map ((==) menuRoute) maybeHoover)
        colorStyle =
            [ if active
                then Attr.style "color" "#E54B4B"
                else Attr.style "color" "#bbb"
            ]
    in
    Html.a
        [ Attr.href (Router.toUrlString (Router.App menuRoute))
        , onMouseOver (Hoover menuRoute)
        , onMouseOut UnHoover
        ]
        [ Grid.row
            [ Row.attrs
                ([ Attr.style "margin" "50px 0px"
                , Attr.style "cursor" "pointer"
                ] ++ if active 
                        then [ Attr.style "border-right" "1px solid #E54B4B" ] 
                        else [])
            ]
            [ Grid.col
                [ Col.attrs colorStyle ]
                [ Icon.viewStyled 
                    [ FAAttr.lg
                    , Attr.style "margin" "0px 50px"
                    ] 
                    (Router.getIcon (Router.App menuRoute))
                , Html.text (Router.toString (Router.App menuRoute))
                ]
            ]
        ]

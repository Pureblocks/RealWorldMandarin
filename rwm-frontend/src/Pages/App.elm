module Pages.App exposing (Model(..), Msg, init, update, view)

import Browser exposing (Document)
import Html exposing (text)
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
import Pages.App.Menu exposing (menu)
import Util exposing (updateWith)

type Model
    = Dashboard PageDashboard.Model
    | Learning PageLearning.Model
    | Training PageTraining.Model
    | Settings PageSettings.Model

type Msg
    = GotDashboardMsg PageDashboard.Msg
    | GotLearningMsg PageLearning.Msg
    | GotTrainingMsg PageTraining.Msg
    | GotSettingsMsg PageSettings.Msg

-- todo push a auth model downstream to init where you always know the user is authenticated
init : Auth -> Router.AppRoute -> ( Model, Cmd Msg)
init auth appRoute = 
    case appRoute of
        Router.Dashboard ->
            PageDashboard.init auth
                |> updateWith Dashboard GotDashboardMsg 

        Router.Learning ->
            PageLearning.init auth
                |> updateWith Learning GotLearningMsg 

        Router.Training ->
            PageTraining.init auth
                        |> updateWith Training GotTrainingMsg

        Router.Settings ->
            PageSettings.init auth
                        |> updateWith Settings GotSettingsMsg 


update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case (msg, model) of
        (GotDashboardMsg subMsg, Dashboard dashboard) ->
            PageDashboard.update subMsg dashboard
                |> updateWith Dashboard GotDashboardMsg

        (GotLearningMsg subMsg, Learning learning) ->
            PageLearning.update subMsg learning
                |> updateWith Learning GotLearningMsg

        (GotTrainingMsg subMsg, Training training) ->
            PageTraining.update subMsg training
                |> updateWith Training GotTrainingMsg

        (GotSettingsMsg subMsg, Settings settings) ->
            PageSettings.update subMsg settings
                |> updateWith Settings GotSettingsMsg
        
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
                        [ menu subRoute
                        , Grid.col
                            [ Col.attrs
                                [ Attr.class "text-center"
                                ]
                            ]
                            (List.map (Html.map toMsg) body)
                        ]
                    ]
                ]
            }
    in
    case model of
        Dashboard m ->
            viewPage PageDashboard.view GotDashboardMsg m Router.Dashboard

        Learning m ->
            viewPage PageLearning.view GotLearningMsg m Router.Learning

        Training m ->
            viewPage PageTraining.view GotTrainingMsg m Router.Training

        Settings m ->
            viewPage PageSettings.view GotSettingsMsg m Router.Settings

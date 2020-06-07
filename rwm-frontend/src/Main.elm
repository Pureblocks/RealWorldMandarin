module Main exposing (main)

import Browser exposing (Document)
import Html as Html
import Json.Decode as Decode
import Models.ElmSeed exposing (elmSeedDecoder)
import Browser.Navigation as Nav
import Url exposing (Url)
import Browser as Browser
import Pages.Home as PageHome
import Pages.Login as PageLogin
import Pages.Dashboard as PageDashboard
import Pages.Learning as PageLearning
import Pages.Training as PageTraining
import Pages.Settings as PageSettings
import Pages.NotFound as PageNotFound
import Auth as Auth
import Router exposing (Route)

type Model
    = Home PageHome.Model
    | Login PageLogin.Model
    | Dashboard PageDashboard.Model
    | Learning PageLearning.Model
    | Training PageTraining.Model
    | Settings PageSettings.Model
    | NotFound PageNotFound.Model

type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url
    | GotHomeMsg PageHome.Msg
    | GotLoginMsg PageLogin.Msg
    | GotDashboardMsg PageDashboard.Msg
    | GotLearningMsg PageLearning.Msg
    | GotTrainingMsg PageTraining.Msg
    | GotSettingsMsg PageSettings.Msg
    | GotNotFoundMsg PageNotFound.Msg

view : Model -> Document Msg
view model =
    let 
        viewPage pageView toMsg subModel =
            let 
                { title, body } =
                    pageView subModel
            in 
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model of
        Home m ->
            viewPage PageHome.view GotHomeMsg m

        Login m ->
            viewPage PageLogin.view GotLoginMsg m

        Dashboard m ->
            viewPage PageDashboard.view GotDashboardMsg m

        Learning m ->
            viewPage PageLearning.view GotLearningMsg m

        Training m ->
            viewPage PageTraining.view GotTrainingMsg m

        Settings m ->
            viewPage PageSettings.view GotSettingsMsg m

        NotFound m ->
            viewPage PageNotFound.view GotNotFoundMsg m

fromAuth : (Auth.Auth -> a) -> Model -> a
fromAuth f model =
    case model of
        Home m ->
            f m.auth

        Login m ->
            f m.auth

        Dashboard m ->
            f m.auth

        Learning m ->
            f m.auth

        Training m ->
            f m.auth

        Settings m ->
            f m.auth

        NotFound m ->
            f m.auth

update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case (msg, model) of
        (ClickedLink urlRequest, _) ->
            case urlRequest of
                Browser.External href -> 
                    ( model, Nav.load href )
                Browser.Internal url ->
                    ( model, Nav.pushUrl (fromAuth Auth.getNavKey model) (Url.toString url))
        (ChangedUrl url, _) ->
            changeRouteTo (Router.fromUrl url) model
            
        (GotHomeMsg subMsg, Home home) ->
            PageHome.update subMsg home
                |> updateWith Home GotHomeMsg

        (GotLoginMsg subMsg, Login login) ->
            PageLogin.update subMsg login
                |> updateWith Login GotLoginMsg

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

        (GotNotFoundMsg subMsg, NotFound notFound) ->
            PageNotFound.update subMsg notFound
                |> updateWith NotFound GotNotFoundMsg

        (_, _) ->
            Debug.log "Received message for the wrong page" ( model, Cmd.none )

updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

changeRouteTo : Route -> Model -> ( Model, Cmd Msg )
changeRouteTo route model =
    let auth = fromAuth identity model
    in
    case route of
        Router.Home -> 
            PageHome.init auth
                |> updateWith Home GotHomeMsg 
        Router.Login ->
            Auth.fold
                (\ _ k -> -- todo push a auth model downstream to init where you always know the user is authenticated
                    ( model, Nav.pushUrl k ( Router.toString Router.Dashboard )) 
                )
                (\k -> 
                    PageLogin.init auth
                        |> updateWith Login GotLoginMsg 
                )
                auth

        Router.Dashboard ->
            Auth.fold
                (\ _ _ -> -- todo push a auth model downstream to init where you always know the user is authenticated
                    PageDashboard.init auth
                        |> updateWith Dashboard GotDashboardMsg 
                )
                (\k -> 
                    ( model, Nav.pushUrl k ( Router.toString Router.Login )) 
                )
                auth
            
        Router.Learning ->
            Auth.fold
                (\ _ _ -> -- todo push a auth model downstream to init where you always know the user is authenticated
                    PageLearning.init auth
                        |> updateWith Learning GotLearningMsg 
                )
                (\k -> 
                    ( model, Nav.pushUrl k ( Router.toString Router.Login )) 
                )
                auth
            
        Router.Training ->
            Auth.fold
                (\ _ _ -> -- todo push a auth model downstream to init where you always know the user is authenticated
                    PageTraining.init auth
                        |> updateWith Training GotTrainingMsg 
                )
                (\k -> 
                    ( model, Nav.pushUrl k ( Router.toString Router.Login )) 
                )
                auth
            
        Router.Settings ->
            Auth.fold
                (\ _ _ -> -- todo push a auth model downstream to init where you always know the user is authenticated
                    PageSettings.init auth
                        |> updateWith Settings GotSettingsMsg 
                )
                (\k -> 
                    ( model, Nav.pushUrl k ( Router.toString Router.Login )) 
                )
                auth
            
        Router.NotFound ->
            PageNotFound.init auth
                |> updateWith NotFound GotNotFoundMsg    

init : Decode.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init appSeedJson currentUrl key = case Decode.decodeValue elmSeedDecoder appSeedJson of
    Ok appSeed ->
        let seedRoute    = Router.fromSeed appSeed
            currentRoute = Router.fromUrl currentUrl
            auth         = Auth.fromMaybeUsername appSeed.user key
        in 
        if seedRoute == currentRoute
            then 
                case currentRoute of
                    Router.Home ->
                        changeRouteTo Router.Home (Home { auth = auth })
                    
                    Router.Login ->
                        changeRouteTo Router.Login (Login (PageLogin.emptyModel auth))
                    
                    Router.Dashboard ->
                        changeRouteTo Router.Dashboard (Dashboard { auth = auth })
                    
                    Router.Learning ->
                        changeRouteTo Router.Learning (Learning { auth = auth })
                    
                    Router.Training ->
                        changeRouteTo Router.Training (Training { auth = auth })
                    
                    Router.Settings ->
                        changeRouteTo Router.Settings (Settings { auth = auth })
                    
                    Router.NotFound ->
                        changeRouteTo Router.NotFound (NotFound { auth = auth })
            
            else
                case seedRoute of 
                    Router.Home ->
                       (Home { auth = auth }, Nav.pushUrl key ( Router.toString seedRoute ))
                    
                    Router.Login ->
                        (Login (PageLogin.emptyModel auth), Nav.pushUrl key ( Router.toString seedRoute ))
                    
                    Router.Dashboard ->
                        (Dashboard { auth = auth }, Nav.pushUrl key ( Router.toString seedRoute ))
                    
                    Router.Learning ->
                        (Learning { auth = auth }, Nav.pushUrl key ( Router.toString seedRoute ))
                    
                    Router.Training ->
                        (Training { auth = auth }, Nav.pushUrl key ( Router.toString seedRoute ))
                    
                    Router.Settings ->
                        (Settings { auth = auth }, Nav.pushUrl key ( Router.toString seedRoute ))
                   
                    Router.NotFound ->
                        (NotFound { auth = auth }, Nav.pushUrl key ( Router.toString seedRoute ))
            
    Err _ -> ( Home { auth = Auth.fromKey key }, Cmd.none )

main : Program Decode.Value Model Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        , subscriptions = subscriptions
        , update = update
        , view = view
        }

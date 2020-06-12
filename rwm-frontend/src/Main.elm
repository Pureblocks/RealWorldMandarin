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
import Pages.App as PageApp
import Pages.NotFound as PageNotFound
import Auth as Auth
import Router as Router
import Util exposing (updateWith)
import Pages.App.Learning as Learning

type Model
    = Home PageHome.Model
    | Login PageLogin.Model
    | App PageApp.Model
    | NotFound PageNotFound.Model

type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url
    | GotHomeMsg PageHome.Msg
    | GotLoginMsg PageLogin.Msg
    | GotAppMsg PageApp.Msg
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

        App m ->
            viewPage PageApp.view GotAppMsg m

        NotFound m ->
            viewPage PageNotFound.view GotNotFoundMsg m

fromAuth : (Auth.Auth -> a) -> Model -> a
fromAuth f model =
    case model of
        Home m ->
            f m.auth

        Login m ->
            f m.auth

        App appModel ->
            case appModel.subModel of
                PageApp.Dashboard m ->
                    f m.auth

                PageApp.Learning m ->
                    f m.auth

                PageApp.Training m ->
                    f m.auth

                PageApp.Settings m ->
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

        (GotAppMsg subMsg, App app) ->
            PageApp.update subMsg app
                |> updateWith App GotAppMsg
            
        (GotHomeMsg subMsg, Home home) ->
            PageHome.update subMsg home
                |> updateWith Home GotHomeMsg

        (GotLoginMsg subMsg, Login login) ->
            PageLogin.update subMsg login
                |> updateWith Login GotLoginMsg

        (GotNotFoundMsg subMsg, NotFound notFound) ->
            PageNotFound.update subMsg notFound
                |> updateWith NotFound GotNotFoundMsg

        (_, _) ->
            Debug.log "Received message for the wrong page in Main" ( model, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

changeRouteTo : Router.Route -> Model -> ( Model, Cmd Msg )
changeRouteTo route model =
    let auth = fromAuth identity model
    in
    case route of
        Router.Home -> 
            PageHome.init auth
                |> updateWith Home GotHomeMsg

        Router.Login ->
            Auth.fold
                (\ _ _ k -> -- todo push a auth model downstream to init where you always know the user is authenticated
                    ( model, Nav.pushUrl k ( Router.toUrlString (Router.App Router.Dashboard) )) 
                )
                (\k -> 
                    PageLogin.init auth
                        |> updateWith Login GotLoginMsg 
                )
                auth

        Router.App subRoute ->
            Auth.fold
                (\ _  _ k -> -- todo push a auth model (type alias AppAuth { username, userid }) downstream to init where you always know the user is authenticated
                    PageApp.init auth subRoute
                        |> updateWith App GotAppMsg 
                )
                (\k -> 
                    PageLogin.init auth
                        |> updateWith Login GotLoginMsg 
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
            auth         = Auth.fromMaybeUserJWT appSeed.userJWT key
        in 
        if seedRoute == currentRoute
            then 
                case currentRoute of
                    Router.Home ->
                        changeRouteTo Router.Home (Home { auth = auth })
                    
                    Router.Login ->
                        changeRouteTo Router.Login (Login (PageLogin.emptyModel auth))
                    
                    Router.App Router.Dashboard ->
                        changeRouteTo 
                            (Router.App Router.Dashboard) 
                            (App { currentHoover = Nothing, subModel = PageApp.Dashboard { auth = auth } })
                    
                    Router.App Router.Learning ->
                        changeRouteTo 
                            (Router.App Router.Learning) 
                            (App { currentHoover = Nothing, subModel = PageApp.Learning (Learning.emptyModel auth) })
                    
                    Router.App Router.Training ->
                        changeRouteTo
                            (Router.App Router.Training)
                            (App { currentHoover = Nothing, subModel = PageApp.Training { auth = auth } })
                    
                    Router.App Router.Settings ->
                        changeRouteTo 
                            (Router.App Router.Settings)
                            (App { currentHoover = Nothing, subModel = PageApp.Settings { auth = auth } })
                    
                    Router.NotFound ->
                        changeRouteTo Router.NotFound (NotFound { auth = auth })
            
            else
                case seedRoute of 
                    Router.Home ->
                       (Home { auth = auth }, Nav.pushUrl key ( Router.toUrlString seedRoute ))
                    
                    Router.Login ->
                        (Login (PageLogin.emptyModel auth), Nav.pushUrl key ( Router.toUrlString seedRoute ))
                    
                    Router.App Router.Dashboard ->
                        ( App { currentHoover = Nothing, subModel = PageApp.Dashboard { auth = auth } }
                        , Nav.pushUrl key ( Router.toUrlString seedRoute ))
                    
                    Router.App Router.Learning ->
                        (App { currentHoover = Nothing, subModel = PageApp.Learning (Learning.emptyModel auth) }
                        , Nav.pushUrl key ( Router.toUrlString seedRoute ))
                    
                    Router.App Router.Training ->
                        (App { currentHoover = Nothing, subModel = PageApp.Training { auth = auth } }
                        , Nav.pushUrl key ( Router.toUrlString seedRoute ))
                    
                    Router.App Router.Settings ->
                        (App { currentHoover = Nothing, subModel = PageApp.Settings { auth = auth } }
                        , Nav.pushUrl key ( Router.toUrlString seedRoute ))
                   
                    Router.NotFound ->
                        (NotFound { auth = auth }, Nav.pushUrl key ( Router.toUrlString seedRoute ))
            
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

module Main exposing (main)

import Browser exposing (Document)
import Html exposing (text)
import Json.Decode as Decode
import Models.ElmSeed exposing (ElmSeed, elmSeedDecoder)
import Browser.Navigation as Nav
import Url exposing (Url)
import Browser as Browser
import Url.Parser as Parser exposing ((</>), Parser, s, string)
import Pages.Home as PageHome
import Pages.Login as PageLogin
import Pages.Dashboard as PageDashboard
import Pages.Learning as PageLearning
import Pages.Training as PageTraining
import Pages.Settings as PageSettings
import Pages.NotFound as PageNotFound

type alias Model =
    { page : Page
    , key : Nav.Key
    , user : Maybe String
    }

type Page
    = Home
    | Login
    | Dashboard
    | Learning
    | Training
    | Settings
    | NotFound

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
    case model.page of
        Home -> 
            Debug.todo "Implement Home"

        Login -> 
            Debug.todo "Implement Login"

        Dashboard -> 
            Debug.todo "Implement Dashboard"

        Learning -> 
            Debug.todo "Implement Learning"

        Training -> 
            Debug.todo "Implement Training"

        Settings -> 
            Debug.todo "Implement Settings"

        NotFound -> 
            Debug.todo "Implement NotFound"

update : Msg -> Model -> ( Model, Cmd Msg)
update mainMsg model =
    case mainMsg of
        ClickedLink urlRequest ->
            case urlRequest of
                Browser.External href -> 
                    ( model, Nav.load href )
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url))
        ChangedUrl url ->
            case urlToPage url of
                Home -> 
                    ( { model | page = Home }, Cmd.none )
                
                Login ->
                    case model.user of
                        Just _ ->
                            ( model, Nav.pushUrl model.key ( pageToUrl Dashboard ))     
                        
                        Nothing ->  
                            ( { model | page = Login }, Cmd.none )
                page -> 
                    case model.user of 
                        Just u -> -- use us later as seed for the init function of the page
                            ( { model | page = page }, Cmd.none )
                        Nothing ->
                            ( model, Nav.pushUrl model.key ( pageToUrl Login ))
        GotHomeMsg msg ->
            Debug.todo "Implement GotHomeMsg"

        GotLoginMsg msg ->
            Debug.todo "Implement GotLoginMsg"

        GotDashboardMsg msg ->
            Debug.todo "Implement GotDashboardMsg"

        GotLearningMsg msg ->
            Debug.todo "Implement GotLearningMsg"

        GotTrainingMsg msg ->
            Debug.todo "Implement GotTrainingMsg"

        GotSettingsMsg msg ->
            Debug.todo "Implement GotSettingsMsg"

        GotNotFoundMsg msg ->
            Debug.todo "Implement GotNotFoundMsg"


urlToPage : Url -> Page
urlToPage url =
    Parser.parse parser url
        |> Maybe.withDefault NotFound

parser : Parser (Page -> a) a
parser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map Login (s "app" </> s "login")
        , Parser.map Dashboard (s "app" </> s "dashboard")
        , Parser.map Learning (s "app" </> s "learning")
        , Parser.map Training (s "app" </> s "training")
        , Parser.map Settings (s "app" </> s "settings")
        ]

pageToUrl : Page -> String
pageToUrl page =
    case page of
        Home -> ""
        Login -> "/app/login"
        Dashboard -> "/app/dashboard"
        Learning -> "/app/learning"
        Training -> "/app/training"
        Settings -> "/app/settings"
        NotFound -> ""

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

pageFromString : String -> Page
pageFromString s =
    case s of
        "Home"      -> Home
        "Login"     -> Login
        "Dashboard" -> Dashboard
        "Learning"  -> Learning
        "Training"  -> Training
        "Settings"  -> Settings
        _           -> NotFound

init : Decode.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init appSeedJson currentUrl key = case Decode.decodeValue elmSeedDecoder appSeedJson of
    Ok appSeed -> 
        let seedPage    = pageFromString appSeed.page
            currentPage = urlToPage currentUrl
            urlCommand  = if seedPage == currentPage 
                            then Cmd.none 
                            else Nav.pushUrl key ( pageToUrl seedPage )
        in ( { page = seedPage, user = appSeed.user, key = key }, urlCommand )
    Err _ -> ( { page = Home, user = Nothing, key = key }, Cmd.none )

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

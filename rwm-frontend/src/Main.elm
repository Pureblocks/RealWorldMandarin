module Main exposing (main)

import Html exposing (Html, div, text, h1, h2, h4, img)
import Html as HTML
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Browser.Navigation as Navigation
import Browser exposing (UrlRequest)
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((</>), Parser, s, top)
import Bootstrap.Navbar as Navbar
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Html.Events exposing (onSubmit, onInput)
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Button as Button
import Bootstrap.Modal as Modal
import Color exposing (Color)
import Debug exposing (log)

-- Colours: https://coolors.co/e54b4b-ffa987-f7ebe8-444140-1e1e24

lightRed : Color
lightRed = Color.hsl 0 0.75 0.60

type alias Flags =
    {}

type alias User =
    { username : String
    , email : String
    , jwt : String
    }

type alias LoginForm =
    { username : String
    , password : String
    }

type alias RegisterForm =
    { username    : String
    , email       : String
    , passwordOne : String
    , passwordTwo : String
    }

type alias Model =
    { navKey : Navigation.Key
    , page : Page
    , navState : Navbar.State
    , modalVisibility : Modal.Visibility
    , user : Maybe User
    , loginForm : LoginForm
    , registerForm : RegisterForm
    }

-- | TODO add login
type Page
    = Home
    | Dashboard
    | HonourHanzi
    | NotFound


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = UrlChange
        }

init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( navState, navCmd ) =
            Navbar.initialState NavMsg

        ( model, urlCmd ) =
            urlUpdate url { navKey = key
                          , navState = navState
                          , page = Home
                          , modalVisibility= Modal.hidden
                          , user = Nothing 
                          , loginForm = LoginForm "" ""
                          , registerForm = RegisterForm "" "" "" ""
                          }
    in
        ( model, Cmd.batch [ urlCmd, navCmd ] )



type Msg
    = UrlChange Url
    | ClickedLink UrlRequest
    | NavMsg Navbar.State
    | CloseModal
    | ShowModal
    | UpdateLoginUsername String
    | UpdateLoginPassword String
    | Login
    | UpdateRegisterUsername String
    | UpdateRegisterEmail String
    | UpdateRegisterPasswordOne String
    | UpdateRegisterPasswordTwo String
    | Register


subscriptions : Model -> Sub Msg
subscriptions model =
    Navbar.subscriptions model.navState NavMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink req ->
             case req of
                 Browser.Internal url ->
                     ( model, Navigation.pushUrl model.navKey <| Url.toString url )

                 Browser.External href ->
                     ( model, Navigation.load href )


        UrlChange url ->
            urlUpdate url model

        NavMsg state ->
            ( { model | navState = state }
            , Cmd.none
            )

        CloseModal ->
            ( { model | modalVisibility = Modal.hidden }
            , Cmd.none
            )

        ShowModal ->
            ( { model | modalVisibility = Modal.shown }
            , Cmd.none
            )

        UpdateLoginUsername username ->
            ( { model | loginForm = LoginForm username model.loginForm.password }
            , Cmd.none 
            )

        UpdateLoginPassword password ->
            ( { model | loginForm = LoginForm model.loginForm.username password }
            , Cmd.none
            )

        Login -> 
            log (model.loginForm.username ++ model.loginForm.password) ( model
            , Cmd.none
            )

        UpdateRegisterUsername username -> 
            let form = model.registerForm
                registerFormUpdated = { form | username = username }
            in ( { model | registerForm = registerFormUpdated}, Cmd.none )

        UpdateRegisterEmail email -> 
            let form = model.registerForm
                registerFormUpdated = { form | email = email }
            in ( { model | registerForm = registerFormUpdated}, Cmd.none )

        UpdateRegisterPasswordOne password -> 
            let form = model.registerForm
                registerFormUpdated = { form | passwordOne = password }
            in ( { model | registerForm = registerFormUpdated}, Cmd.none )

        UpdateRegisterPasswordTwo password -> 
            let form = model.registerForm
                registerFormUpdated = { form | passwordTwo = password }
            in ( { model | registerForm = registerFormUpdated}, Cmd.none )

        Register ->
            log "Registering" ( model , Cmd.none )


urlUpdate : Url -> Model -> ( Model, Cmd Msg )
urlUpdate url model =
    case model.user of
        Nothing ->
            ( { model | page = Home }, Cmd.none )

        Just user ->
            case decode url of
                Nothing ->
                    ( { model | page = NotFound }, Cmd.none )

                Just route ->
                    ( { model | page = route }, Cmd.none )


decode : Url -> Maybe Page
decode = UrlParser.parse routeParser


routeParser : Parser (Page -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map Home top
        , UrlParser.map Home (s "index.html")
        , UrlParser.map Dashboard (s "dashboard")
        , UrlParser.map HonourHanzi (s "honour-hanzi")
        ]


view : Model -> Browser.Document Msg
view model =
    case model.page of
        Home -> 
            { title = "Welcome to Real World Mandarin"
            , body = [ div [] (pageHome model) ]
            }

        _ -> 
            { title = "Real World Mandarin"
            , body =
                [ div []
                    [ topMenu model
                    , mainContent model
                    , modal model
                    ]
                ]
            }

pageHome : Model -> List (Html Msg)
pageHome model = 
    [ Grid.container []
        [ Grid.row []
            [ Grid.col 
                [] 
                [ h4 [] [ text "Login" ]
                , loginForm model
                ]
            , Grid.col 
                []
                [ h4 [] [ text "Register" ]
                , registerForm model
                ]
            ]
        ]
    ]

loginForm : Model -> Html Msg
loginForm model = 
    Form.form 
        [ onSubmit Login, action "javascript:void(0);" ]
        [ Form.group 
            [ Form.attrs 
                [ value model.loginForm.username
                , onInput (\v -> UpdateLoginUsername v)
                ]
            ]
            [ Form.label [for "username"] [ text "Username"]
            , Input.text [ Input.id "username" ]
            ]
        , Form.group 
            [ Form.attrs 
                [ value model.loginForm.password
                , onInput (\v -> UpdateLoginPassword v)
                ]
            ]
            [ Form.label [for "mypwd"] [ text "Password"]
            , Input.password [ Input.id "mypwd" ]
            ]
        , Button.submitButton
            [ Button.primary ] 
            [ text "Login" ]
        ]

registerForm : Model -> Html Msg
registerForm model =
    Form.form
        [ onSubmit Register, action "javascript:void(0);" ]
        [ Form.group
            [ Form.attrs 
                [ value model.registerForm.username
                , onInput (\v -> UpdateRegisterUsername v)
                ]
            ]
            [ Form.label [for "registerUsername"] [ text "Username"]
            , Input.text [ Input.id "registerUsername" ]
            ]
        , Form.group
            [ Form.attrs 
                [ value model.registerForm.email
                , onInput (\v -> UpdateRegisterEmail v)
                ]
            ]
            [ Form.label [for "registerEmail"] [ text "E-Mail"]
            , Input.email [ Input.id "registerEmail" ]
            ]
        , Form.group 
            [ Form.attrs 
                [ value model.registerForm.passwordOne
                , onInput (\v -> UpdateRegisterPasswordOne v)
                ]
            ]
            [ Form.label [for "passwordOne"] [ text "Password"]
            , Input.password [ Input.id "passwordOne" ]
            ]
        , Form.group 
            [ Form.attrs 
                [ value model.registerForm.passwordTwo
                , onInput (\v -> UpdateRegisterPasswordTwo v)
                ]
            ]
            [ Form.label [for "passwordTwo"] [ text "Repeat password"]
            , Input.password [ Input.id "passwordTwo" ]
            ]
        , Button.submitButton
            [ Button.primary ] 
            [ text "Register" ]
        ]

topMenu : Model -> Html Msg
topMenu model =
    Navbar.config NavMsg
        |> Navbar.withAnimation
        |> Navbar.darkCustom lightRed
        |> Navbar.brand [ href "/" ] [ text "Real World Mandarin" ]
        |> Navbar.customItems
            [ Navbar.customItem (
                div 
                    [ class "dropdown" ] 
                    [ HTML.a 
                        [ id "dropdownMenuLink" 
                        , class "dropdown-toggle"
                        , style "color" "white"
                        , href "#" 
                        , attribute "data-toggle" "dropdown" 
                        , attribute "role" "button" 
                        , attribute "aria-haspopup" "true" 
                        , attribute "aria-expanded" "false"
                        ] 
                        [ text "Hello, Thomas!" 
                        , img 
                            [ src "https://s3.eu-central-1.amazonaws.com/bootstrapbaymisc/blog/24_days_bootstrap/fox.jpg"
                            , width 30
                            , height 30
                            , class "rounded-circle"
                            , style "margin-left" "10px"
                            ] 
                            []
                        ]
                    , div 
                        [ class "dropdown-menu"
                        , style "margin-top" "10px"
                        , attribute "aria-labelledby" "dropdownMenuLink"
                        ] 
                        [ HTML.a
                            [ class "dropdown-item"
                            , href "/"
                            ]
                            [ text "Dashboard" ]
                        , HTML.a
                            [ class "dropdown-item"
                            , href "/settings"
                            ]
                            [ text "Settings" ]
                        , div [ class "dropdown-divider" ] []
                        , HTML.a
                            [ class "dropdown-item"
                            , href "/logout"
                            ]
                            [ text "Logout" ]
                        ]
                    ]
            )]
        |> Navbar.view model.navState

mainContent : Model -> Html Msg
mainContent model =
    Grid.container [] <|
        case model.page of
            Dashboard ->
                pageDashboard model

            HonourHanzi ->
                pageHonourHanzi model

            NotFound ->
                pageNotFound

            Home ->
                pageHome model


pageDashboard : Model -> List (Html Msg)
pageDashboard model =
    [ Grid.row [ Row.attrs [ style "margin-top" "20px" ] ]
        [ Grid.col []
            [ Card.config [ Card.outlineDanger ]
                |> Card.headerH4 [] [ text "Honour Hanzi" ]
                |> Card.block []
                    [ Block.text [] [ text "Check out the Honour Hanzi module!" ]
                    , Block.custom <|
                        Button.linkButton
                            [ Button.primary, Button.attrs [ href "/honour-hanzi" ] ]
                            [ text "Start the Hanzi" ]
                    ]
                |> Card.view
            ]
        ]
    ]


pageHonourHanzi : Model -> List (Html Msg)
pageHonourHanzi model =
    [ h2 [] [ text "Honour Hanzi" ]
    , Button.button
        [ Button.success
        , Button.large
        , Button.block
        , Button.attrs [ onClick ShowModal ]
        ]
        [ text "Click me" ]
    ]


pageNotFound : List (Html Msg)
pageNotFound =
    [ h1 [] [ text "Not found" ]
    , text "SOrry couldn't find that page"
    ]


modal : Model -> Html Msg
modal model =
    Modal.config CloseModal
        |> Modal.small
        |> Modal.h4 [] [ text "Getting started ?" ]
        |> Modal.body []
            [ Grid.containerFluid []
                [ Grid.row []
                    [ Grid.col
                        [ Col.xs6 ]
                        [ text "Col 1" ]
                    , Grid.col
                        [ Col.xs6 ]
                        [ text "Col 2" ]
                    ]
                ]
            ]
        |> Modal.view model.modalVisibility
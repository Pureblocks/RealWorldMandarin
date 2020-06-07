module Pages.Login exposing (Model, Msg, init, update, view, emptyModel)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (text)
import Auth exposing (Auth)
import Html exposing (Html, div, text, h1, h2, h5, img, a, span, node)
import Html.Events exposing (onClick)
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Modal as Modal
import Html.Events exposing (onSubmit, onInput)
import Bootstrap.Button as Button
import Html.Attributes exposing (..)
import Http
import Clients.AuthAPI exposing (postApiAuthRegister, postApiAuthLogin)
import Clients.Models.AuthAPI exposing (Register, Login, UserJWT)
import Auth as Auth

type alias Model =
    { auth : Auth.Auth
    , loginForm : LoginForm
    , registerForm : RegisterForm
    , modalVisibility : Modal.Visibility
    }

type alias LoginForm =
    { username : String
    , password : String
    }

emptyLoginForm: LoginForm
emptyLoginForm =
    { username = ""
    , password = ""
    }

type alias RegisterForm =
    { username    : String
    , email       : String
    , passwordOne : String
    , passwordTwo : String
    }

emptyRegisterForm: RegisterForm
emptyRegisterForm =
    { username = ""
    , email = ""
    , passwordOne = ""
    , passwordTwo = ""
    }

toLogin : LoginForm -> Login
toLogin lf =
    { loginUsername = lf.username
    , password = lf.password
    }

toRegister : RegisterForm -> Register
toRegister rf =
    { registerUsername = (rf.username)
    , email = (rf.email)
    , passwordOne = (rf.passwordOne)
    , passwordTwo = (rf.passwordTwo)    
    }

emptyModel : Auth -> Model
emptyModel auth =
    { auth = auth
    , loginForm = emptyLoginForm
    , registerForm = emptyRegisterForm
    , modalVisibility = Modal.hidden
    }

type Msg
    = UpdateLoginUsername String
    | UpdateLoginPassword String
    | SubmitLogin
    | LoginResponse (Result (Http.Error , Maybe { metadata : Http.Metadata, body : String }) UserJWT)
    | FailureLogin
    | ShowModal
    | CloseModal
    | UpdateRegisterUsername String
    | UpdateRegisterEmail String
    | UpdateRegisterPasswordOne String
    | UpdateRegisterPasswordTwo String
    | SubmitRegister
    | RegisterResponse (Result (Http.Error , Maybe { metadata : Http.Metadata, body : String }) UserJWT)
    | FailureRegister

init : Auth.Auth -> ( Model, Cmd Msg)
init auth = ( emptyModel auth
           , Cmd.none 
           )

update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case msg of
        UpdateLoginUsername username ->
            ( { model | loginForm = LoginForm username model.loginForm.password }
            , Cmd.none 
            )

        UpdateLoginPassword password ->
            ( { model | loginForm = LoginForm model.loginForm.username password }
            , Cmd.none
            )

        SubmitLogin -> 
            ( model
            , Cmd.map LoginResponse (postApiAuthLogin (toLogin model.loginForm)))

        LoginResponse result ->
            case result of
                Ok userJWT ->
                    ( { model | auth = Auth.Authenticated userJWT.un (Auth.getNavKey model.auth) }
                    , Nav.pushUrl (Auth.getNavKey model.auth) "/app/dashboard"
                    )

                Err (error, maybeReason) -> 
                    Debug.log ("Failure login: " ++ Maybe.withDefault "" (Maybe.map (\x -> x.body) maybeReason)) ( model, Cmd.none )

        FailureLogin ->
            Debug.log "Failure login" ( model, Cmd.none )

        ShowModal -> 
            ( { model | modalVisibility = Modal.shown }
            , Cmd.none
            )

        CloseModal ->
            ( { model | modalVisibility = Modal.hidden }
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

        SubmitRegister ->
            ( model
            , Cmd.map RegisterResponse (postApiAuthRegister (toRegister model.registerForm))
            )

        RegisterResponse result ->
            case result of
                Ok userJWT -> 
                    ( { model | auth = Auth.Authenticated userJWT.un (Auth.getNavKey model.auth) }
                    , Nav.pushUrl (Auth.getNavKey model.auth) "/app/dashboard"
                    )

                Err (error, maybeReason) -> 
                    Debug.log ("Failure registration: " ++ Maybe.withDefault "" (Maybe.map (\x -> x.body) maybeReason)) ( model, Cmd.none )
            

        FailureRegister ->
            Debug.log "Failure registration" ( model, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "Real World Mandarin - Login"
    , body = loginBody model
    }

loginBody : Model -> List (Html Msg)
loginBody model =
    [ Grid.container []
        [ Grid.row []
            [ Grid.col 
                [ Col.sm12, Col.md6 ]
                []
            , Grid.col 
                [ Col.sm12, Col.md6 ] 
                [ h1 [] [ text "Welcome" ]
                , h2 [] [ text "Sign in to Honour Hanzi"]
                , h5 [] [ text "Enter Details" ]
                , loginForm model
                , div 
                    [ class "center-block" ]
                    [ node 
                        "center"
                        []
                        [ span
                            []
                            [text "Don't have an account? "
                            , span
                                [ onClick ShowModal ]
                                [ text "Sign up" ]
                            ]
                        ]
                    ]
                , modal model
                ]
            ]
        ]
    ]

loginForm : Model -> Html Msg
loginForm model = 
    Form.form 
        [ onSubmit SubmitLogin, action "javascript:void(0);" ]
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
            [ Button.primary, Button.large, Button.block ] 
            [ text "Login" ]
        ]

modal : Model -> Html Msg
modal model =
    Modal.config CloseModal
        |> Modal.large
        |> Modal.h4 [] [ text "Register to Honour Hanzi!" ]
        |> Modal.body []
            [ Grid.container []
                [ Grid.row []
                    [ Grid.col
                        [ Col.xs12 ]
                        [ registerForm model ]
                    ]
                ]
            ]
        |> Modal.view model.modalVisibility

registerForm : Model -> Html Msg
registerForm model =
    Form.form
        [ onSubmit SubmitRegister, action "javascript:void(0);" ]
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
            [ Button.primary, Button.large, Button.block ] 
            [ text "Register" ]
        ]

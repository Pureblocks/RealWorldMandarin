module Pages.App.Learning exposing (Model, Msg, init, update, view, emptyModel)

import Browser exposing (Document)
import Auth exposing (Auth(..))
import Router exposing (Route(..), AppRoute(..))
import Html as Html
import Html.Attributes as Attr
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid as Grid
import Bootstrap.Form.Textarea as Textarea
import Html.Events exposing (onMouseOver, onMouseOut, onClick)
import Clients.Models.CharacterAPI exposing(Character)
import Clients.CharacterAPI exposing(getApiCharactersUserByUserIdNext, postApiCharactersUsersByUserIdByCharacterId)

type alias Model =
    { auth: Auth
    , story: String
    , character: Maybe Character
    , errorMessage: Maybe String
    }

emptyModel : Auth -> Model
emptyModel auth =
    { auth = auth
    , story = ""
    , character = Nothing
    , errorMessage = Nothing
    }

type Msg
    = NextCharacter
    | ReceiveCharacter CharacterResponse
    | StoryAreaInput String
    | SubmitStory

type CharacterResponse
    = GotCharacter Character
    | GotCharacterError String 

init : Auth -> ( Model, Cmd Msg)
init auth = ( emptyModel auth, nextCharacter auth )

nextCharacter: Auth -> Cmd Msg
nextCharacter auth = 
    case auth of
        Authenticated _ userId _ ->
            Cmd.map
                (\result ->
                    case result of
                        Result.Err _ ->
                            ReceiveCharacter (GotCharacterError "Error TODO")
                        Result.Ok char ->
                            ReceiveCharacter (GotCharacter char)
                )
                (getApiCharactersUserByUserIdNext userId)
        
        Guest _ ->
            Cmd.none

update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case msg of
        NextCharacter ->
            ( model, nextCharacter model.auth )

        ReceiveCharacter characterResponse ->
            case characterResponse of
                GotCharacter character -> 
                    ( { model | character = Just character, story = "" }, Cmd.none )

                GotCharacterError errorMessage ->
                    ( { model | errorMessage = Just errorMessage }, Cmd.none )

        StoryAreaInput story ->
            ( { model | story = story }, Cmd.none )
        
        SubmitStory ->
            case (model.auth, model.character) of
                (Authenticated _ userId _, Just character) ->
                    let cmd = Cmd.map
                            (\result ->
                                case result of
                                    Result.Err _ ->
                                        ReceiveCharacter (GotCharacterError "Error TODO")
                                    Result.Ok _ ->
                                        NextCharacter
                            )
                            ( postApiCharactersUsersByUserIdByCharacterId
                                userId
                                character.characterId
                                { story = model.story }
                            )
                    in ( model, cmd )

                (_, _) ->
                    ( model, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "Real World Mandarin - Learning"
    , body = 
        case model.character of
            Nothing ->
                []
            
            Just character ->
                [ Grid.row
                    []
                    [ Grid.col
                        [ Col.attrs
                            [ Attr.class "d-none d-sm-block"
                            , Attr.style "margin-top" "50px"
                            ]
                        ]
                        []
                    ]
                , Grid.row
                    []
                    [ Grid.col
                        [ Col.xs12
                        , Col.lg6
                        , Col.attrs
                            [ Attr.style "margin-bottom" "10px"
                            , Attr.style "color" "#1E1E24"
                            , Attr.class "text-center"
                            ] 
                        ]
                        [ Html.div
                            [ Attr.style "font-size" "42px"
                            ]
                            [ Html.span [ Attr.style "color" "1E1E24"] [ Html.text character.keyword ]
                            ]
                        ]
                    , Grid.col
                        [ Col.xs12
                        , Col.lg6
                        ]
                        []
                    ]
                , Grid.row
                    [ Row.attrs [ Attr.style "padding" "25px" ]]
                    [ Grid.col
                        [ Col.xs12
                        , Col.lg6
                        , Col.attrs
                            [ Attr.style "margin" "0px 0px 0px 0px"
                            , Attr.style "color" "#1E1E24"
                            , Attr.class "text-center"
                            , Attr.style "border-top" "1px solid #1E1E24"
                            , Attr.style "border-bottom" "1px solid #1E1E24"
                            ] 
                        ]
                        [ Html.div
                            [ Attr.style "font-size" "247px" ]
                            [ Html.text character.hanzi ]
                        ]
                    , Grid.col
                        [ Col.xs12
                        , Col.lg6
                        ]
                        [ Textarea.textarea
                            [ Textarea.id "storyArea"
                            , Textarea.onInput StoryAreaInput
                            , Textarea.value model.story
                            , Textarea.attrs
                                [ Attr.placeholder "Enter your story here..."
                                , Attr.style "background-color" "#F6F4FC"
                                , Attr.style "color" "#1E1E24"
                                , Attr.style "font-size" "18px"
                                , Attr.style "border" "none"
                                , Attr.style "height" "100%"
                                ]
                            ]
                        ]
                    ]
                , Grid.row
                    [ Row.attrs
                        [ Attr.style "margin-top" "20px" ]
                    ]
                    [ Grid.col
                        [ Col.xs12
                        , Col.lg6
                        , Col.attrs
                            [ Attr.style "margin" "0px 0px 0px 0px"
                            , Attr.style "color" "#444140"
                            , Attr.class "text-center"
                            ] 
                        ]
                        [ Html.div
                            [ Attr.style "font-size" "32px" ]
                            ( List.map
                                (\e -> Html.text (" " ++ e.elementKeyword ++ " " ++ e.elemHanzi ++ " "))
                                character.elements
                            )
                        ]
                    , Grid.col
                        [ Col.xs12
                        , Col.lg6
                        ]
                        [ Grid.row
                            []
                            [ Grid.col
                                [ Col.xs12 ]
                                [ Html.div
                                    [ Attr.class "float-right"
                                    , Attr.style "background-color" "#E54B4B"
                                    , Attr.style "font-size" "24px"
                                    , Attr.style "padding" "10px"
                                    , Attr.style "margin-right" "25px"
                                    , Attr.style "color" "#EAE9EE"
                                    , Attr.style "border-radius" "5px"
                                    , Attr.style "border" "1px solid #131021"
                                    , Attr.style "cursor" "pointer"
                                    , onClick SubmitStory
                                    ]
                                    [ Html.text "Learned" ]
                                ]
                            ]
                        ]
                    ]
                ] 
    }

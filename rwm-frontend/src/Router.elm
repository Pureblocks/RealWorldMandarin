module Router exposing (Route(..), AppRoute(..), parser, fromSeed, fromUrl, toUrlString, toString, getIcon)

import Url.Parser as Parser exposing ((</>), Parser, oneOf, s)
import Html exposing (a)
import Url exposing (Url)
import Models.ElmSeed exposing (ElmSeed)
import FontAwesome.Icon as Icon
import FontAwesome.Solid as FA

type Route
    = Home
    | Login
    | App AppRoute
    | NotFound

type AppRoute
    = Dashboard
    | Learning
    | Training
    | Settings

parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map Login (s "app" </> s "login")
        , Parser.map (App Dashboard) (s "app" </> s "dashboard")
        , Parser.map (App Learning) (s "app" </> s "learning")
        , Parser.map (App Training) (s "app" </> s "training")
        , Parser.map (App Settings) (s "app" </> s "settings")
        ]

fromSeed : ElmSeed -> Route
fromSeed seed =
    case seed.page of
        "Home" ->
            Home

        "Login" ->
            Login

        "Dashboard" ->
            (App Dashboard)

        "Learning" ->
            (App Learning)

        "Training" ->
            (App Training)

        "Settings" ->
            (App Settings)

        _ ->
            NotFound

fromUrl : Url -> Route
fromUrl url =
    Parser.parse parser url
        |> Maybe.withDefault NotFound

toUrlString : Route -> String
toUrlString route =
    case route of
        Home ->
            ""

        Login ->
            "/app/login"

        (App Dashboard) ->
            "/app/dashboard"

        (App Learning) ->
            "/app/learning"

        (App Training) ->
            "/app/training"

        (App Settings) ->
            "/app/settings"

        NotFound ->
            ""

toString : Route -> String
toString route =
    case route of
        Home ->
            "Home"

        Login ->
            "Login"

        (App Dashboard) ->
            "Dashboard"

        (App Learning) ->
            "Learning"

        (App Training) ->
            "Training"

        (App Settings) ->
            "Settings"

        NotFound ->
            "NotFound"

getIcon : Route -> Icon.Icon
getIcon route =
    case route of
        Home ->
            FA.home

        Login ->
            FA.signInAlt

        (App Dashboard) ->
            FA.chartLine

        (App Learning) ->
            FA.graduationCap

        (App Training) ->
            FA.gamepad

        (App Settings) ->
            FA.wrench

        NotFound ->
            FA.questionCircle
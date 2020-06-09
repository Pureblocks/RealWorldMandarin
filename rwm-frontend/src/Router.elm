module Router exposing (Route(..), parser, fromSeed, fromUrl, toUrlString, toString, getIcon)

import Url.Parser as Parser exposing ((</>), Parser, oneOf, s)
import Html exposing (a)
import Url exposing (Url)
import Models.ElmSeed exposing (ElmSeed)
import FontAwesome.Icon as Icon
import FontAwesome.Solid as FA

type Route
    = Home
    | Login
    | Dashboard
    | Learning
    | Training
    | Settings
    | NotFound

parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map Login (s "app" </> s "login")
        , Parser.map Dashboard (s "app" </> s "dashboard")
        , Parser.map Learning (s "app" </> s "learning")
        , Parser.map Training (s "app" </> s "training")
        , Parser.map Settings (s "app" </> s "settings")
        ]

fromSeed : ElmSeed -> Route
fromSeed seed =
    case seed.page of
        "Home" ->
            Home

        "Login" ->
            Login

        "Dashboard" ->
            Dashboard

        "Learning" ->
            Learning

        "Training" ->
            Training

        "Settings" ->
            Settings

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

        Dashboard ->
            "/app/dashboard"

        Learning ->
            "/app/learning"

        Training ->
            "/app/training"

        Settings ->
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

        Dashboard ->
            "Dashboard"

        Learning ->
            "Learning"

        Training ->
            "Training"

        Settings ->
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

        Dashboard ->
            FA.chartLine

        Learning ->
            FA.graduationCap

        Training ->
            FA.gamepad

        Settings ->
            FA.wrench

        NotFound ->
            FA.questionCircle
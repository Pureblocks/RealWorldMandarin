module Router exposing (Route(..), parser, fromSeed, fromUrl, toString)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)
import Html exposing (a)
import Url exposing (Url)
import Models.ElmSeed exposing (ElmSeed)

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

toString : Route -> String
toString route =
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

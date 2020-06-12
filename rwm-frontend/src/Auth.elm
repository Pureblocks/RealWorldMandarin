module Auth exposing (Auth(..), isAuthenticated, getNavKey, getUserName, fromKey, fromMaybeUserJWT, fold)

import Browser.Navigation as Nav
import Clients.Models.AuthAPI exposing (UserJWT)

type Auth
    = Authenticated String Int Nav.Key
    | Guest Nav.Key

isAuthenticated : Auth -> Bool
isAuthenticated a =
    case a of
        Authenticated _ _ _ -> True
        Guest _ -> False

getNavKey : Auth -> Nav.Key
getNavKey a =
    case a of
        Authenticated _ _ k -> k
        Guest k           -> k

getUserName : Auth -> Maybe String
getUserName a =
    case a of
        Authenticated u _ _ -> Just u
        Guest _           -> Nothing

fromKey : Nav.Key -> Auth
fromKey = Guest

fromMaybeUserJWT : Maybe UserJWT -> Nav.Key -> Auth
fromMaybeUserJWT userJWT key =
    case userJWT of
        Just u -> Authenticated u.un u.sub key
        Nothing -> Guest key

fold : (String -> Int -> Nav.Key -> a) -> (Nav.Key -> a) -> Auth -> a
fold onAuthenticated onGuest auth =
    case auth of
        Authenticated un uid k -> onAuthenticated un uid k
        Guest k -> onGuest k
module Auth exposing (Auth(..), isAuthenticated, getNavKey, getUserName, fromKey, fromMaybeUsername, fold)

import Browser.Navigation as Nav

type Auth
    = Authenticated String Nav.Key
    | Guest Nav.Key

isAuthenticated : Auth -> Bool
isAuthenticated a =
    case a of
        Authenticated _ _ -> True
        Guest _ -> False

getNavKey : Auth -> Nav.Key
getNavKey a =
    case a of
        Authenticated _ k -> k
        Guest k           -> k

getUserName : Auth -> Maybe String
getUserName a =
    case a of
        Authenticated u _ -> Just u
        Guest _           -> Nothing

fromKey : Nav.Key -> Auth
fromKey = Guest

fromMaybeUsername : Maybe String -> Nav.Key -> Auth
fromMaybeUsername username key =
    case username of
        Just u -> Authenticated u key
        Nothing -> Guest key

fold : (String -> Nav.Key -> a) -> (Nav.Key -> a) -> Auth -> a
fold onAuthenticated onGuest auth =
    case auth of
        Authenticated un k -> onAuthenticated un k
        Guest k -> onGuest k
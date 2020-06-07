{-# LANGUAGE DataKinds, TypeOperators #-}

module HttpAPI.API
    ( app
    ) where

import Servant
import Servant.Auth.Server
import Database.Beam.Postgres (Connection)
import HttpAPI.WordADayAPI
import HttpAPI.AuthAPI
import Configuration.Config
import HttpAPI.FrontEnd

type API auths = WordADayAPI
      :<|> AuthAPI
      :<|> (Auth auths UserJWT :> FrontEndAPI)
      :<|> Raw

server :: Connection 
       -> CookieSettings
       -> JWTSettings 
       -> Server (API auths)
server conn cs jwtCfg = wordADayServer conn 
                   :<|> loginServer conn cs jwtCfg 
                   :<|> frontEndAPIServer
                   :<|> serveDirectoryFileServer "resources/web/"

api :: Proxy (API '[Cookie])
api = Proxy

app :: Connection -> Config -> Application
app conn conf = 
    let jwtCfg = defaultJWTSettings (jwtKey conf)
        cookieSettings = defaultCookieSettings 
            { cookieIsSecure = NotSecure
            , cookieXsrfSetting = Nothing 
            }
        cfg    = cookieSettings :. jwtCfg :. EmptyContext
    in serveWithContext api cfg (server conn cookieSettings jwtCfg)

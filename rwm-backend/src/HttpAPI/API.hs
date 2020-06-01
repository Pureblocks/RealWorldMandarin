{-# LANGUAGE DataKinds, TypeOperators #-}

module HttpAPI.API
    ( app
    ) where

import Servant
import Servant.Auth.Server
import Database.Beam.Postgres (Connection)
import HttpAPI.WordADayAPI
import HttpAPI.LoginAPI
import Configuration.Config

type API = "api" :> WordADayAPI
     :<|> "api" :> LoginAPI

server :: Connection 
       -> CookieSettings
       -> JWTSettings 
       -> Server API
server conn cs jwtCfg = wordADayServer conn 
                   :<|> loginServer conn cs jwtCfg 

api :: Proxy API
api = Proxy

app :: Connection -> Config -> Application
app conn conf = 
    let jwtCfg = defaultJWTSettings (jwtKey conf)
        cfg    = defaultCookieSettings :. jwtCfg :. EmptyContext
    in serveWithContext api cfg (server conn defaultCookieSettings jwtCfg)
module HttpAPI.HTML where

import Data.ByteString.Lazy as Lazy
import Network.HTTP.Media ((//), (/:))
import Servant.API (Accept(..), MimeRender(..))
import Lucid
import Data.Aeson (ToJSON, encode)
import qualified Data.Text as T
import Data.ByteString.Lazy (toStrict)
import Data.Text.Encoding (decodeUtf8)

data HTML = HTML

newtype RawHtml = RawHtml { unRaw :: Lazy.ByteString }

instance Accept HTML where
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
    mimeRender _ = unRaw

renderElmApp :: ToJSON a => a -> Html ()
renderElmApp elmSeed = html_ $ do
    head_ $ do 
        meta_ [charset_ "utf-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1, shrink-to-fit=no"]
        link_ [rel_ "stylesheet", type_ "text/css", href_ bootstrapCss]
        link_ [rel_ "stylesheet", type_ "text/css", href_ karla]
        -- TODO when using Text and T.append it is unable to resolve the correct type
        termWith "style" [type_ "text/css"]  "body { font-family: Karla !important; } a:link {text-decoration: none  !important; border: 0 !important; } a:visited {text-decoration: none !important; border: 0 !important; } a:active {text-decoration: none !important; border: 0; } a:hover {text-decoration: none !important; border: 0; }"
        termWith "script" [type_ "application/javascript", src_ jQuery] ""
        termWith "script" [type_ "application/javascript", src_ popper] ""
        termWith "script" [type_ "application/javascript", src_ bootstrapJs] ""
        termWith "script" [type_ "application/javascript", src_ elmJs] ""
    body_ $ script_  ("var app = Elm.Main.init({ flags: " `T.append` decodeUtf8 
                        (toStrict $ encode elmSeed)  `T.append` " })")
    where
        bootstrapCss = "https://stackpath.bootstrapcdn.com/bootstrap/4.5.0/css/bootstrap.min.css"
        karla = "https://fonts.googleapis.com/css?family=Karla"
        jQuery = "https://code.jquery.com/jquery-3.5.1.slim.min.js"
        popper = "https://cdn.jsdelivr.net/npm/popper.js@1.16.0/dist/umd/popper.min.js"
        bootstrapJs = "https://stackpath.bootstrapcdn.com/bootstrap/4.5.0/js/bootstrap.min.js"
        elmJs = "/js/main.js"
           






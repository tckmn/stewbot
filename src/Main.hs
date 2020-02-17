{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Monad
import Data.Function
import Data.List
import Data.Maybe
import System.Directory
import qualified Data.ByteString as B hiding (pack, unpack)
import qualified Data.ByteString.Char8 as B (pack, unpack)
import qualified Data.ByteString.Internal as B (c2w, w2c)
import qualified Data.ByteString.Lazy as BL hiding (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as BL (pack, unpack)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL (encodeUtf8)

import Stewbot
import System.Process

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200, status404)
import Text.Hamlet
import Text.Blaze.Html.Renderer.Text (renderHtml)

main = makeBot >>= run 3000 . app

resp ct = responseLBS status200 [("Content-Type", ct)]

respHtml :: Html -> Response
respHtml = resp "text/html" . TL.encodeUtf8 . renderHtml

respPlain :: BL.ByteString -> Response
respPlain = resp "text/plain"

notfound :: Response
notfound = responseLBS status404 [("Content-Type", "text/plain")] "not found"

headContent = [shamlet|
    <title>Stewbot
    <meta charset=utf-8>
    <link rel=stylesheet href=/static/main.css>
    |]

header = [shamlet|
    <header>
        <a href=/>home
        <a href=/search>search
    |]

app :: Stewbot -> Application
app bot req respond = case liftM2 (,) requestMethod pathInfo req of

    ("GET", []) -> getHistory >>= \history ->
        respond $ respHtml $(shamletFile "html/home.hamlet")

    ("GET", ["search"]) -> respond $ respHtml $(shamletFile "html/search.hamlet")

    ("GET", path@("static":_)) -> do
        let fname = T.unpack $ T.intercalate "/" path
        let ct = guessCt fname
        exists <- doesFileExist fname
        if exists then BL.readFile fname >>= respond . resp ct else respond notfound

    ("POST", ["search"]) -> strictRequestBody req >>= runSearch bot >>= respond . respPlain
    ("POST", ["progress"]) -> getProgress >>= respond . respPlain

    _ -> respond notfound
    where guessCt fname
            | ".html" `isSuffixOf` fname = "text/html"
            | ".css"  `isSuffixOf` fname = "text/css"
            | ".js"   `isSuffixOf` fname = "application/javascript"
            | ".png"  `isSuffixOf` fname = "image/png"
            | otherwise                  = "text/plain"

-- oldstuffs

oldoldmain = do
    bot <- makeBot
    res <- addItems bot
        [Item "item_79003957"   1 Best
        ,Item "item_679798180"  1 Best
        ,Item "item_78959876"   1 Best
        ,Item "item_79088941"   1 Best
        ,Item "item_78969823"   2 Best
        ,Item "item_78971804"   3 Best
        ,Item "item_78971249"   1 Best
        ,Item "item_78971557"   3 Best
        ,Item "item_83013875"   6 No
        ,Item "item_79019981"   8 No
        ,Item "item_1368015104" 8 No
        ,Item "item_169853881"  2 No
        ,Item "item_79020062"   1 No
        ,Item "item_130991374"  1 No
        ,Item "item_747904715"  1 No
        ,Item "item_78966249"   3 No
        ]
    putStrLn . show $ res

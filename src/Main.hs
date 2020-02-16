{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

import Stewbot
import System.Process

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200, status404)
import Text.Hamlet
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Text.Lazy.Encoding (encodeUtf8)
import System.Directory (doesFileExist)
import Data.List
import Control.Monad

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as B hiding (pack, unpack)
import qualified Data.ByteString.Char8 as B (pack, unpack)
import qualified Data.ByteString.Internal as B (c2w, w2c)
import qualified Data.ByteString.Lazy as BL hiding (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as BL (pack, unpack)

main = makeBot >>= run 3000 . app

resp ct = responseLBS status200 [("Content-Type", ct)]

respHtml :: Html -> Response
respHtml = resp "text/html" . encodeUtf8 . renderHtml

notfound :: Response
notfound = responseLBS status404 [("Content-Type", "text/plain")] "not found"

headContent = [shamlet|
    <title>Stewbot
    <meta charset=utf-8>
    <link rel=stylesheet href=static/main.css>
    |]

header = [shamlet|
    <header>
        <a href=/>home
        <a href=/search>search
    |]

app :: Stewbot -> Application
app bot req respond = case liftM2 (,) requestMethod pathInfo req of

    ("GET", [])         -> respond $ respHtml $(shamletFile "html/home.hamlet")
    ("GET", ["search"]) -> respond $ respHtml $(shamletFile "html/search.hamlet")

    ("GET", ["static", path]) -> do
        let fname = "static/" ++ T.unpack path
        let ct = guessCt fname
        exists <- doesFileExist fname
        if exists then BL.readFile fname >>= respond . resp ct else respond notfound

    _ -> respond notfound
    where guessCt fname
            | ".html" `isSuffixOf` fname = "text/html"
            | ".css"  `isSuffixOf` fname = "text/css"
            | ".js"   `isSuffixOf` fname = "application/javascript"
            | ".png"  `isSuffixOf` fname = "image/png"
            | otherwise                  = "text/plain"

-- oldstuffs

oldmain = do
    bot <- makeBot
    runSearch bot "list" >>= writeFile "out.html"
    callCommand "firefox out.html"

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

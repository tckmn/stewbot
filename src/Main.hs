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

main = makeBot >>= run 3000 . app

resp :: Html -> Response
resp = responseLBS status200 [("Content-Type", "text/html")] . encodeUtf8 . renderHtml

notfound :: Response
notfound = responseLBS status404 [("Content-Type", "text/plain")] "not found"

headContent = [shamlet|
    <title>Stewbot
    <meta charset='utf-8'>
    |]

header = [shamlet|
    <p>HEADER
    |]

app bot req respond = respond $
    case requestMethod req of
      "GET"  -> case pathInfo req of
                  [] -> resp $(shamletFile "html/home.hamlet")
                  _ -> notfound
      "POST" -> case pathInfo req of
                  _ -> notfound

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

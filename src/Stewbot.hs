{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Stewbot (
    makeBot, addItems, runSearch, Item(Item), Replacement(..)
) where

import Control.Lens hiding ((.=))
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Data.Quantities
import Data.List
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (urlEncode)
import Network.Wreq
import System.Directory (doesFileExist)
import qualified Data.ByteString as B hiding (pack, unpack)
import qualified Data.ByteString.Char8 as B (pack, unpack)
import qualified Data.ByteString.Internal as B (c2w, w2c)
import qualified Data.ByteString.Lazy as BL hiding (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as BL (pack, unpack)
import qualified Network.Wreq.Session as S

insta = ("https://www.instacart.com/" ++)
uenc = B.unpack . urlEncode False . B.pack

data Stewbot = Stewbot { sess :: S.Session
                       , cid :: String
                       , ckey :: String
                       } deriving Show

data Item = Item { item_id :: String
                 , quantity :: Int
                 , repl :: Replacement
                 } deriving Show
instance ToJSON Item where
    toJSON Item{item_id,quantity,repl} = object $
        (case repl of
           Best    -> []
           No      -> ["replacement_policy".=no]
           Use rid -> ["replacement_policy".=use, "replacement_item_id".=rid]
        ) ++ ["item_id".=item_id, "quantity".=quantity]
            where no  = "no_replacements" :: String
                  use = "users_choice"    :: String

data Replacement = Best | No | Use Int deriving Show

data SearchResult = SearchResult { search_id :: String
                                 , name :: String
                                 , size :: String
                                 , image :: String
                                 , price :: String
                                 , prev :: Bool
                                 , feat :: Bool
                                 } deriving Show
instance FromJSON SearchResult where
    parseJSON (Object v) = SearchResult <$>
        v .: "id" <*>
        v .: "name" <*>
        v .: "size" <*>
        (v .: "image" >>= (.: "url")) <*>
        (v .: "pricing" >>= (.: "price")) <*>
        (elem "previously_purchased" <$> attr) <*>
        (elem "featured_badge_gray" <$> attr)
            where attr = v .: "attributes" :: Parser [String]

-- needed for login
-- should be able to find this in source code of instacart homepage, if something changes
csrfToken :: S.Session -> IO (B.ByteString)
csrfToken sess = extractToken <$> findToken <$> S.get sess (insta "")
    where findToken = snd . B.breakSubstring ct . BL.toStrict . (^. responseBody)
          extractToken = B.takeWhile nq . B.drop 1 . B.dropWhile nq . B.drop (B.length ct + 1)
          ct = "csrf-token"
          nq = (/= B.c2w '"')

-- this is only called if there isn't a file called "session" with the saved session
login :: S.Session -> IO (S.Session)
login sess = do
    token <- csrfToken sess
    creds <- lines <$> readFile "credentials"
    S.post sess (insta "v3/dynamic_data/authenticate/login") [
        "authenticity_token" := token,
        "grant_type" := ("password" :: B.ByteString),
        "email" := (creds !! 0),
        "password" := (creds !! 1)
        ]
    return sess

parseResp :: (Object -> Parser a) -> Response BL.ByteString -> a
parseResp x = fromJust . parseMaybe x . decodeObj
    where decodeObj = fromJust . decode . (^. responseBody)

makeBot :: IO (Stewbot)
makeBot = do
    haveSession <- doesFileExist "session"
    sess <- if haveSession
       then read <$> readFile "session" >>= flip S.newSessionControl tlsManagerSettings
       else do
           sess <- S.newSession >>= login
           writeFile "session" . show =<< S.getSessionCookieJar sess
           return sess

    -- find cart id from initial_bundle xhr
    bundle <- S.putWith
        (defaults & header "X-Requested-With" .~ ["XMLHttpRequest"])
        sess (insta "v3/initial_bundle") B.empty
    let cid = parseResp
              (\x -> (x.:"bundle") >>= (.:"current_user") >>= (.:"personal_cart_id"))
              bundle
    let ckey = parseResp
               (\x -> (x.:"bundle") >>= (.:"cache_key"))
               bundle

    return Stewbot{sess,cid,ckey}

addItems :: Stewbot -> [Item] -> IO (Response BL.ByteString)
addItems Stewbot{sess,cid} items = do
    S.putWith
        (defaults & header "X-Requested-With" .~ ["XMLHttpRequest"]
                  & header "Content-Type" .~ ["application/json"])
        sess (insta $ "v3/carts/" ++ cid ++ "/update_items")
        (BL.concat ["{\"items\":",encode items,"}"])

runSearch :: Stewbot -> String -> IO (String)
runSearch bot fname =
    concat <$> sequence [readFile "head.html", body, pure "</body></html>"]
    where body = fmap concat . join $ (mapM (search bot) . lines) <$> readFile fname

search :: Stewbot -> String -> IO (String)
search Stewbot{sess,ckey} item = do
    res <- S.get sess (insta $ "v3/containers/wegmans/search_v3/" ++ uenc item ++ "?cache_key=" ++ ckey)
    -- putStrLn . BL.unpack $ res ^. responseBody
    return $ concat
        [ "<div class='items'>"
        , "<h2>"++item++"</h2>"
        , concat $ render <$> parseResp parser res
        , "</div>"
        ]
    where parser x = (x.:"container") >>= (.:"modules")
                 >>= (filterM (\x -> (isPrefixOf "search_result_set") <$> x.:"id"))
                 <&> head
                 >>= (.:"data") >>= (.:"items") :: Parser [SearchResult]

render :: SearchResult -> String
render SearchResult{search_id,name,size,image,price,prev,feat} = concat
    [ "<div class='item' data-id='"++search_id++"'>"
    , if prev then "<div class='prev'>&nbsp;</div>" else ""
    , if feat then "<div class='feat'>&nbsp;</div>" else ""
    , "<div class='imgcont'>"
    , "<p>"++price++"</p>"
    , "<p>"++size++"</p>"
    , "<img src='"++image++"'>"
    , "</div>"
    , "<div class='namcont'>"++name++"</div>"
    , "</div>"
    ]

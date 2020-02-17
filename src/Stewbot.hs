{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Stewbot (
    Stewbot(..), Item(..), Replacement(..),
    makeBot, addItems, runSearch, getProgress, getHistory
) where

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

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Types
import Data.Char
import Data.Quantities
import Data.Time.Format
import Data.Time.LocalTime
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (urlEncode)
import Network.Wreq
import Numeric
import Text.Hamlet
import Text.Blaze
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Network.Wreq.Session as S

insta = ("https://www.instacart.com/" ++)
searchdir = "static/searches"
uenc = B.unpack . urlEncode False . B.pack
readData      = readFile      . ("data/"++)
readDataBL    = BL.readFile   . ("data/"++)
writeData     = writeFile     . ("data/"++)
doesDataExist = doesFileExist . ("data/"++)

headContent = [shamlet|
    <title>Stewbot
    <meta charset=utf-8>
    <link rel=stylesheet href=/static/main.css>
    |]

-- removes parentheses and excess spaces and lowercases
normalize :: String -> String
normalize = unwords . words . flip helper 0
    where helper []      _ = []
          helper ('(':s) n = helper s (n+1)
          helper (')':s) n = helper s (n-1)
          helper (c:s)   0 = (toLower c):helper s 0
          helper (c:s)   n = helper s n

hush :: Either a b -> Maybe b
hush (Right x) = Just x
hush _         = Nothing

data Stewbot = Stewbot { sess :: S.Session
                       , cid :: String
                       , exclude :: M.Map String [String]
                       , choice :: M.Map String String
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

quant = fromString' d'
    where (Right d') = readDefinitions $ unlines
                        [ defaultDefString
                        , "gal = gallon"
                        , "qt = quart"
                        , "fl = floz/oz"
                        , "ct = 1"
                        ]
type Quant = Either (QuantityError Double) (Quantity Double)

data SearchResult = SearchResult { search_id :: String
                                 , name :: String
                                 , size :: String
                                 , image :: String
                                 , price :: String
                                 , prev :: Bool
                                 , feat :: Bool
                                 , efficiency :: Quant
                                 } deriving Show

instance FromJSON SearchResult where
    parseJSON (Object v) = SearchResult <$>
        v .: "id" <*>
        v .: "name" <*>
        v .: "size" <*>
        (v .: "image" >>= (.: "url")) <*>
        price <*>
        (elem "previously_purchased" <$> attr) <*>
        (elem "featured_badge_gray" <$> attr) <*>
        do price <- price; size <- size
           return $ convertBase <$>
               if "At $" `isPrefixOf` size
                  then quant $ drop 4 size
                  else liftM2 divideQuants (quant $ tail price) (quant $ patch size)
        where attr = v .: "attributes" :: Parser [String]
              size = v .: "size"
              price = v .: "pricing" >>= (.: "price")
              patch ""    = ""
              patch (c:s) = (if c=='x' then '*' else c):patch s

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
    creds <- lines <$> readData "credentials"
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
    haveSession <- doesDataExist "session"
    sess <- if haveSession
       then read <$> readData "session" >>= flip S.newSessionControl tlsManagerSettings
       else do
           sess <- S.newSession >>= login
           writeData "session" . show =<< S.getSessionCookieJar sess
           return sess

    -- find cart id from initial_bundle xhr
    bundle <- S.putWith
        (defaults & header "X-Requested-With" .~ ["XMLHttpRequest"])
        sess (insta "v3/initial_bundle") B.empty
    let cid = parseResp
              (\x -> (x.:"bundle") >>= (.:"current_user") >>= (.:"personal_cart_id"))
              bundle

    -- read file for things to exclude from search results
    exclude <- decodeData "exclude"

    -- and the one for things we got last time
    choice <- decodeData "choice"

    return Stewbot{sess,cid,exclude,choice}

    where decodeData fname = do
            exists <- doesDataExist fname
            fromMaybe M.empty <$> if exists then decode <$> readDataBL fname else mempty

addItems :: Stewbot -> [Item] -> IO (Response BL.ByteString)
addItems Stewbot{sess,cid} items = do
    S.putWith
        (defaults & header "X-Requested-With" .~ ["XMLHttpRequest"]
                  & header "Content-Type" .~ ["application/json"])
        sess (insta $ "v3/carts/" ++ cid ++ "/update_items")
        (BL.concat ["{\"items\":",encode items,"}"])

runSearch :: Stewbot -> BL.ByteString -> IO (BL.ByteString)
runSearch bot list = do
    -- perform the search
    body <- fmap mconcat . mapM (search bot) . zip [0..] . lines $ BL.unpack list

    -- generate a file name
    fname <- formatTime defaultTimeLocale "%F_%T" <$> getZonedTime
    let fname' = searchdir++"/"++fname++".html"

    -- output the data
    writeFile fname' . TL.unpack $ renderHtml $(shamletFile "html/results.hamlet")

    -- remove the progress file
    exists <- doesFileExist "progress"
    when exists $ removeFile "progress"

    return $ BL.pack fname'

search :: Stewbot -> (Integer,String) -> IO (Html)
search Stewbot{sess,exclude,choice} (idx,item') = do
    let item = normalize item'
    req <- S.get sess (insta $ "v3/containers/wegmans/search_v3/" ++ uenc item)

    -- report that we got this item
    -- (lol this means we can't do multiple searches simultaneously but whatever)
    -- FIXME i guess
    writeFile "progress" $ show idx

    -- filter out excluded items
    let res = filter ((`notElem` M.findWithDefault [] item exclude) . search_id) $
                parseResp parser req

    -- try to guess the correct units (based on the naive heuristic -- maybe improve?)
    let guess = join $ fst <$> (M.lookupMax $ foldr f M.empty res)
            where f x acc = M.insertWith (+) (units <$> hush (efficiency x)) 1 acc

    -- now find the most efficient item with those units
    let minid = search_id $ minimumBy (compare `on` safeEfficiency) res
            where safeEfficiency SearchResult{efficiency} = case efficiency of
                    Left _ -> 1/0
                    Right val -> if or $ (units val ==) <$> guess then magnitude val else 1/0

    return [shamlet|
        <div .items>
            <h2>#{item}
            <div .itemswrap>
                ^{mconcat $ render minid (M.lookup item choice) <$> res}
    |]

    where parser x = (x.:"container") >>= (.:"modules")
                 >>= (filterM (\x -> (isPrefixOf "search_result_set") <$> x.:"id"))
                 <&> head
                 >>= (.:"data") >>= (.:"items") :: Parser [SearchResult]

render :: String -> Maybe String -> SearchResult -> Html
render minid choid SearchResult{search_id,name,size,image,price,prev,feat,efficiency} = [shamlet|
    $newline never
    <div .item :good:.chosen data-id=#{search_id}>
        $if prev
            <div .prev>&nbsp;
        $if feat
            <div .feat>&nbsp;
        <div .imgcont>
            <p>#{price}
            <p>#{size}
            <img src=#{image}>
        <div .namcont>
            #{name}
        <div .effcont>
            $case efficiency
                $of Left err
                    #{show err}
                $of Right val
                    #{showVal val}
            $if best
                <span .tag>best
            $if chos
                <span .tag>chosen
    |] where best = search_id == minid
             chos = or ((search_id ==) <$> choid)
             good = search_id == fromMaybe minid choid
             showVal val = showEFloat (Just 2) (magnitude val) (concat . words . show $ units val)

getProgress :: IO (BL.ByteString)
getProgress = do
    exists <- doesFileExist "progress"
    if exists then BL.readFile "progress" else mempty

getHistory :: IO (Html)
getHistory = (mconcat . map f . reverse . sort) <$> listDirectory searchdir
    where f x = let x' = searchdir++"/"++x in [shamlet|
        <li>
            <a href=#{x'}>#{x}|]

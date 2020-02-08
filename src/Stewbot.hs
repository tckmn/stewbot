{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Stewbot (
    makeBot, testeroni
) where

import Data.Maybe
import Data.Aeson.Types
import Control.Lens
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wreq
import System.Directory (doesFileExist)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B (c2w, w2c)
import qualified Data.ByteString.Lazy as BL
import qualified Network.Wreq.Session as S
import Data.Aeson

insta = ("https://www.instacart.com/" ++)

data Stewbot = Stewbot { sess :: S.Session
                       , cid :: String
                       } deriving Show

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
    cid <- return . fromJust $
        parseMaybe (\x -> (x.:"bundle") >>= (.:"current_user") >>= (.:"personal_cart_id"))
        (fromJust . decode $ bundle ^. responseBody :: Object)

    return Stewbot { sess, cid }

testeroni :: Stewbot -> IO String
testeroni Stewbot{sess,cid} = return cid

-- addItem :: S.Session -> IO (Response BL.ByteString)
-- addItem = do
--     S.post sess (insta "v3/carts/" ++ TODO ++ "/update_items") [
--         ]

{-# LANGUAGE OverloadedStrings #-}

module Stewbot (
    makeSession
) where

import Control.Lens
import Network.HTTP.Client (defaultManagerSettings)
import Network.Wreq
import System.Directory (doesFileExist)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B (c2w, w2c)
import qualified Data.ByteString.Lazy as BL
import qualified Network.Wreq.Session as S

insta = ("https://www.instacart.com/" ++)

-- needed for login
-- should be able to find this in source code of instacart homepage, if something changes
csrfToken :: S.Session -> IO (B.ByteString)
csrfToken sess = extractToken <$> findToken <$> S.get sess (insta "")
    where findToken = snd . B.breakSubstring ct . BL.toStrict . (^. responseBody)
          extractToken = B.takeWhile nq . B.drop 1 . B.dropWhile nq . B.drop (B.length ct + 1)
          ct = "csrf-token"
          nq = (/= B.c2w '"')

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

makeSession :: IO (S.Session)
makeSession = do
    haveSession <- doesFileExist "session"
    if haveSession
       then read <$> readFile "session" >>= flip S.newSessionControl defaultManagerSettings
       else do
           sess <- S.newSession >>= login
           writeFile "session" . show =<< S.getSessionCookieJar sess
           return sess

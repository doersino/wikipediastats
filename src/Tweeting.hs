{-# LANGUAGE OverloadedStrings #-}

module Tweeting where

import Config

import qualified Data.Text             as T (unpack, pack)
import qualified Data.ByteString.Char8 as B (pack)
import           Web.Twitter.Conduit

postTweet :: TwitterConfig -> String -> IO ()
postTweet twitterConfig status = do
    let twInfo = setCredential tokens credential def
    mgr <- newManager tlsManagerSettings
    res <- call twInfo mgr $ statusesUpdate $ T.pack status
    return ()
  where
    tokens = twitterOAuth
        { oauthConsumerKey = B.pack $ consumerKey twitterConfig
        , oauthConsumerSecret = B.pack $ consumerSecret twitterConfig
        }
    credential = Credential
        [ ("oauth_token", B.pack $ accessToken twitterConfig)
        , ("oauth_token_secret", B.pack $ accessTokenSecret twitterConfig)
        ]

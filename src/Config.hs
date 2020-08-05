{-# LANGUAGE OverloadedStrings #-}

module Config where

import           Data.Ini.Config
import qualified Data.Text           as T (unpack, pack)

data GeneralConfig = GeneralConfig
    { cacheFile :: FilePath
    , verbosity :: Int
    , limit :: Int
    } deriving (Show)

data TwitterConfig = TwitterConfig
    { consumerKey :: String
    , consumerSecret :: String
    , accessToken :: String
    , accessTokenSecret :: String
    } deriving (Show)

data Config = Config
    { generalConfig :: GeneralConfig
    , twitterConfig :: TwitterConfig
    } deriving (Show)

parseConfig :: IniParser Config
parseConfig = do
    general <- section "GENERAL" $ do
        cacheFile       <- field        "cache_file"
        verbosity       <- fieldOf      "verbosity"       number
        limit <- fieldOf      "limit" number
        return $ GeneralConfig (T.unpack cacheFile) verbosity limit
    twitter <- section "TWITTER" $ do
        consumerKey       <- field "consumer_key"
        consumerSecret    <- field "consumer_secret"
        accessToken       <- field "access_token"
        accessTokenSecret <- field "access_token_secret"
        return $ TwitterConfig (T.unpack consumerKey) (T.unpack consumerSecret) (T.unpack accessToken) (T.unpack accessTokenSecret)
    return $ Config general twitter

parseIni :: FilePath -> Config
parseIni configFile = case (parseIniFile (T.pack configFile) parseConfig) of
                          Left err -> error err
                          Right co -> co

tweetingPossible :: TwitterConfig -> Bool
tweetingPossible tc = (not . null) $ concat $ [ consumerKey tc
                                              , consumerSecret tc
                                              , accessToken tc
                                              , accessTokenSecret tc
                                              ]

{-# LANGUAGE OverloadedStrings #-}

module Lib where
--    ( someFunc
--    , initStats
--    )
-- where

-- TODO clean up and sort imports
import qualified Data.Text           as T (unpack, pack)
import           Network.HTTP.Simple (httpSink, parseRequest)
import           Network.HTTP.Client (parseUrlThrow)
import           Text.HTML.DOM       (sinkDoc)
import           Text.XML.Cursor     (attributeIs, hasAttribute, child, content, element, fromDocument, ($//), (&/), (&//))
import           Text.XML            (Document)
import           Text.Read           (readMaybe)
import           Data.Char           (isDigit)
import           Control.Monad       (when)
import           Text.JSON           (JSON, decode, encode, Result(..), showJSON, readJSON)
import           System.Directory    (doesFileExist)
import           Data.Maybe          (catMaybes, fromMaybe)
import           Data.Map            (Map)
import qualified Data.Map as Map
import           Data.Ini.Config
import           Web.Twitter.Conduit
import qualified Data.ByteString.Char8 as B (pack)


someFunc :: IO ()
someFunc = putStrLn "someFunc"



data Wikipedia = Wikipedia
    { subdomain :: String
    , language :: String
    }
    deriving (Show, Eq, Ord)

instance JSON Wikipedia where
    showJSON w = showJSON [subdomain w, language w]
    readJSON j = case readJSON j of
        Ok [s, l] -> Ok $ Wikipedia {subdomain = s, language = l}
        Error e -> Error e

getWikipedias :: IO [Wikipedia]
getWikipedias = do
    req <- parseUrlThrow "https://meta.wikimedia.org/wiki/List_of_Wikipedias/Table"
    doc <- httpSink req $ const sinkDoc
    let cursor = fromDocument doc
    let rows = cursor
               $// attributeIs "class" "mw-parser-output"
               &// element "table"
               &// element "tr"
    let row = map ($// child &// content) rows
    let langs = init $ map (map T.unpack) $ map (\r -> [r !! 0, r !! 2]) $ filter (not . null) row  -- last row is total, i.e. just numbers, hence "init"
    return $ map (\s -> Wikipedia (s !! 1) (s !! 0)) langs

data WikipediaStats = WikipediaStats
    { wikipedia :: Wikipedia
    , contentPages :: Maybe Integer
    , pages :: Maybe Integer
    , pageEdits :: Maybe Integer
    , registeredUsers :: Maybe Integer
    }
    deriving (Show, Eq)

instance JSON WikipediaStats where
    showJSON ws = showJSON (wikipedia ws, [contentPages ws, pages ws, pageEdits ws, registeredUsers ws])
    readJSON j = case readJSON j of
        Ok (w, [c, p, e, r]) -> Ok $ WikipediaStats {wikipedia = w, contentPages = c, pages = p, pageEdits = e, registeredUsers = r}
        Error e -> Error e


type StatIdentifier = String

statDescriptions :: Map StatIdentifier String
statDescriptions = Map.fromList
    [ ("mw-statistics-articles", "now consists of XXX articles")
    , ("mw-statistics-pages",    "now contains XXX pages including non-articles")
    , ("mw-statistics-edits",    "has now received XXX edits")
    , ("mw-statistics-users",    "has now been shaped by XXX registered users")
    ]

type WikipediaStats2 = Map StatIdentifier Integer
type WikipediasStats2 = Map Wikipedia WikipediaStats2
type WikipediasStatsCompared2 = Map Wikipedia (WikipediaStats2, WikipediaStats2)

getStatsPage :: String -> IO Document
getStatsPage url = do
    req <- parseUrlThrow url
    httpSink req $ const sinkDoc

getStat :: Document -> String -> IO (Maybe Integer)
getStat doc c = do
    let cursor = fromDocument doc
    let stat = cursor
               $// attributeIs "class" (T.pack c)
               &// attributeIs "class" "mw-statistics-numbers"
               &/ content
    return $ sanitize stat
  where
    sanitize stat = readMaybe $ filter isDigit $ (T.unpack . head) stat :: Maybe Integer

getStats :: Wikipedia -> IO WikipediaStats2
getStats w = do
    let url = "https://" ++ subdomain w ++ ".wikipedia.org/wiki/Special:Statistics"
    doc <- getStatsPage url
    let classes = Map.keys statDescriptions
    stats <- mapM (getStat doc) classes
    let stats2 = map (\(c, s) -> case s of
                                    Nothing -> Nothing
                                    Just a -> Just (c, a)) $ zip classes stats
    return $ Map.fromList $ catMaybes stats2

loadStats :: FilePath -> IO (Maybe WikipediasStats2)
loadStats f = do
    exists <- doesFileExist f
    if not exists
        then return Nothing
        else do
            contents <- readFile f
            let r = decode contents
            case r of
                (Error s) -> return Nothing
                (Ok s)    -> return $ Just s

storeStats :: FilePath -> WikipediasStats2 -> IO ()
storeStats f s = do
    writeFile f (encode s)


pairUp :: WikipediasStats2 -> WikipediasStats2 -> WikipediasStatsCompared2
pairUp os ns = Map.intersectionWith (\o n -> (o, n)) os ns

-- compareStats $ fromList [(Wikipedia {subdomain = "zu", language = "Zulu"}, (fromList [("mw-statistics-articles",4842),("mw-statistics-edits",91354),("mw-statistics-pages",8433),("mw-statistics-users",13375)],fromList [("mw-statistics-articles",5001),("mw-statistics-edits",91354),("mw-statistics-pages",8433),("mw-statistics-users",13375)]))]
compareStats :: WikipediasStatsCompared2 -> WikipediasStats2
compareStats c = Map.map (\(o, n) ->
                            Map.filter (/= 0) $ Map.intersectionWith leadingDigitChanged o n) c
  where
    --leadingDigitChanged ov nv = case nv - ov of
    leadingDigitChanged ov nv =  if (head $ show nv) == (head $ show ov) --for testing: if (last $ show nv) == (last $ show ov)
        then 0
        else read $ (head $ show nv) : (take (length (tail $ show nv)) $ repeat '0')

-- TODO can make this function a bit more point-free
prettyComparedStats :: WikipediasStats2 -> [String]
prettyComparedStats s = concatMap Map.elems $ Map.elems $ Map.mapWithKey (\w c ->
                            Map.mapWithKey (\cl dif ->
                                prettyTweetText w cl dif) c) s

-- TODO convert zeros into natural language in some cases and sometimes insert spaces: 3 million, 300 000
prettyTweetText :: Wikipedia -> String -> Integer -> String
prettyTweetText w cl n = "The " ++ language w ++ " edition of Wikipedia " ++ replaceXXX (fromMaybe "" $ Map.lookup cl statDescriptions) (show n) ++ ", check out more stats here: https://" ++ subdomain w ++ ".wikipedia.org/wiki/Special:Statistics"

replaceXXX :: String -> String -> String
replaceXXX ('X':'X':'X':xs) s = s ++ replaceXXX xs s
replaceXXX (x:xs)           s = x : replaceXXX xs s
replaceXXX ""               s = ""



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

tweetingPossible :: TwitterConfig -> Bool
tweetingPossible tc = (not . null) $ concat $ [ consumerKey tc
                                              , consumerSecret tc
                                              , accessToken tc
                                              , accessTokenSecret tc
                                              ]

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

-- TODO modularize: types, scraping/jsoning, tweeting, processing, main/app
-- TODO error handling: https://stackoverflow.com/questions/6009384/exception-handling-in-haskell
-- TODO decent test coverage for data manipulation functions, use https://hspec.github.io

testing :: IO ()
testing = do
    configFile <- readFile "config.ini"
    let config = case (parseIniFile (T.pack configFile) parseConfig) of
                    Left err -> error err
                    Right co -> co
    let generalConf = generalConfig config
    let twitterConf = twitterConfig config
    let cachePath = cacheFile generalConf
    let notSilent = verbosity generalConf > 0
    let verbose   = verbosity generalConf == 2
    when notSilent $ putStrLn "Successfully parsed configuration."
    when verbose $ putStrLn $ show config

    --postTweet twitterConf "Test post please ignore"

    when notSilent $ putStr "Loading previous stats... "
    oldS <- loadStats cachePath
    when notSilent $ putStrLn "done."
    when verbose $ putStrLn $ show oldS

    when notSilent $ putStr "Getting list of Wikipedias... "
    w <- getWikipedias
    w <- return $ take (limit generalConf) w
    let m = length w
    when notSilent $ putStrLn $ "got " ++ show m ++ "."
    when verbose $ putStrLn $ show w

    let numberedWikipedias = zip [0..] w
    s2 <- mapM (\(n, w) -> do
        when notSilent $ putStr $ "Getting stats for " ++ subdomain w ++ ".wikipedia.org (" ++ show n ++ "/" ++ show m ++ ")... "
        s <- getStats w
        when notSilent $ putStrLn $ "done."
        when verbose $ putStrLn $ show s
        return s
        ) numberedWikipedias
    let s = Map.fromList $ zip w s2

    case oldS of
        Just os -> do
            -- TODO status msg
            let pairs = pairUp os s
            let comp = compareStats pairs
            when verbose $ putStrLn $ show comp
            -- TODO status msg
            let pret = prettyComparedStats comp
            when verbose $ mapM_ putStrLn pret

            -- TODO limit tweets to like 3 each run to avoid cracking any api limits?
            if tweetingPossible twitterConf
                then do
                    -- TODO statusmsg
                    -- TODO tweeting (maybe limit to one tweet per period: the one belonging to the highest-ranking wiki)
                    mapM_ (postTweet twitterConf) pret
                else do
                    -- TODO statusmsg
                    when notSilent $ putStrLn "Tweeting is disabled since not all API keys and secrets have been specified."
                    return ()
        Nothing -> do
            -- TODO status msg
            return ()

    -- TODO tweet

    -- TODO store the largest tweeted value as well, in order to avoid duplicates when a bunch of stuff is added, deleted, then added again?
    when notSilent $ putStr "Storing updated stats... "
    storeStats cachePath s
    when notSilent $ putStrLn "done."

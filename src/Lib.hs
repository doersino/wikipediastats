{-# LANGUAGE OverloadedStrings #-}

module Lib where
--    ( someFunc
--    , initStats
--    )
-- where

import           Data.Text           (unpack, pack)
import           Network.HTTP.Simple (httpSink, parseRequest)
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
import Data.Ini.Config


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
    req <- parseRequest "https://meta.wikimedia.org/wiki/List_of_Wikipedias/Table"
    doc <- httpSink req $ const sinkDoc
    let cursor = fromDocument doc
    let rows = cursor
               $// attributeIs "class" "mw-parser-output"
               &// element "table"
               &// element "tr"
    let row = map ($// child &// content) rows
    let langs = init $ map (map unpack) $ map (\r -> [r !! 0, r !! 2]) $ filter (not . null) row  -- last row is total, i.e. just numbers, hence "init"
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
    req <- parseRequest url
    httpSink req $ const sinkDoc

getStat :: Document -> String -> IO (Maybe Integer)
getStat doc c = do
    let cursor = fromDocument doc
    let stat = cursor
               $// attributeIs "class" (pack c)
               &// attributeIs "class" "mw-statistics-numbers"
               &/ content
    return $ sanitize stat
  where
    sanitize stat = readMaybe $ filter isDigit $ (unpack . head) stat :: Maybe Integer

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
    leadingDigitChanged ov nv = if (head $ show nv) == (head $ show ov)
        then 0
        else read $ (head $ show nv) : (take (length (tail $ show nv)) $ repeat '0')
        -- TODO already filter out zeros here

-- TODO can make this function a bit more point-free
prettyComparedStats :: WikipediasStats2 -> [String]
prettyComparedStats s = concatMap Map.elems $ Map.elems $ Map.mapWithKey (\w c ->
                            Map.mapWithKey (\cl dif ->
                                prettyTweetText w cl dif) c) s

prettyTweetText :: Wikipedia -> String -> Integer -> String
prettyTweetText w cl n = "The " ++ language w ++ " edition of Wikipedia " ++ replaceXXX (fromMaybe "" $ Map.lookup cl statDescriptions) (show n) ++ ", check out more stats here: https://" ++ subdomain w ++ ".wikipedia.org/wiki/Special:Statistics"

replaceXXX :: String -> String -> String
replaceXXX ('X':'X':'X':xs) s = s ++ replaceXXX xs s
replaceXXX (x:xs)           s = x : replaceXXX xs s
replaceXXX ""               s = ""

-- TODO config loading (verbosity, path to cache file)
-- TODO tweeting (limit to one tweet per period: the one belonging to the highest-ranking wiki)
-- TODO tests, https://hspec.github.io

-- https://docs.haskellstack.org/en/stable/README/
-- https://hackage.haskell.org/package/html-conduit
-- http://hackage.haskell.org/package/twitter-conduit

verbose :: Bool
verbose = True

statsPath :: FilePath
statsPath = "test.json"

limitToNLargest :: Int
limitToNLargest = 9999

-- TODO adts for these, see https://hackage.haskell.org/package/config-ini-0.2.4.0/docs/Data-Ini-Config.html
parseConfig :: IniParser ((String, Int, Int), (String, String, String, String))
parseConfig = do
    general <- section "GENERAL" $ do
        cacheFile       <- field        "cacheFile"
        verbosity       <- fieldOf      "verbosity"       number
        limitToNLargest <- fieldOf      "limitToNLargest" number
        return (unpack cacheFile, verbosity, limitToNLargest)
    twitter <- section "TWITTER" $ do
        consumerKey       <- field "consumer_key"
        consumerSecret    <- field "consumer_secret"
        accessToken       <- field "access_token"
        accessTokenSecret <- field "access_token_secret"
        return (unpack consumerKey, unpack consumerSecret, unpack accessToken, unpack accessTokenSecret)
    return (general, twitter)

testing :: IO ()
testing = do
    configFile <- readFile "config.ini"
    let config = parseIniFile (pack configFile) parseConfig
    putStrLn $ show config
    -- TODO config loading

    putStr "Loading previous stats... "
    oldS <- loadStats statsPath
    putStrLn "done."
    when verbose $ putStrLn $ show oldS

    putStr "Getting list of Wikipedias... "
    w <- getWikipedias
    w <- return $ take limitToNLargest w
    let m = length w
    putStrLn $ "got " ++ show m ++ "."
    when verbose $ putStrLn $ show w

    let numberedWikipedias = zip [0..] w
    s2 <- mapM (\(n, w) -> do
        putStr $ "Getting stats for " ++ subdomain w ++ ".wikipedia.org (" ++ show n ++ "/" ++ show m ++ ")... "
        s <- getStats w
        putStrLn $ "done."
        when verbose $ putStrLn $ show s
        return s
        ) numberedWikipedias
    let s = Map.fromList $ zip w s2

    case oldS of
        Just os -> do
            let pairs = pairUp os s
            let comp = compareStats pairs
            putStrLn $ show comp
            let pret = prettyComparedStats comp
            mapM_ putStrLn pret
        Nothing -> return ()

    putStr "Storing updated stats... "
    storeStats statsPath s
    putStrLn "done."

    -- TODO tweet


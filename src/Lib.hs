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
import           Data.Maybe          (fromMaybe)


someFunc :: IO ()
someFunc = putStrLn "someFunc"



data Wikipedia = Wikipedia
    { subdomain :: String
    , language :: String
    }
    deriving (Show, Eq)

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

getStats :: Wikipedia -> IO WikipediaStats
getStats w = do
    let url = "https://" ++ subdomain w ++ ".wikipedia.org/wiki/Special:Statistics"
    doc <- getStatsPage url
    cp <- getStat doc "mw-statistics-articles"
    p <- getStat doc "mw-statistics-pages"
    pe <- getStat doc "mw-statistics-edits"
    ru <- getStat doc "mw-statistics-users"
    return $ WikipediaStats w cp p pe ru

loadStats :: FilePath -> IO (Maybe [WikipediaStats])
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

storeStats :: FilePath -> [WikipediaStats] -> IO ()
storeStats f s = do
    writeFile f (encode s)


-- TODO test this, it seems iffy and is also dumb
pairUp :: [WikipediaStats] -> [WikipediaStats] -> [(WikipediaStats, WikipediaStats)]
pairUp os ns = fromMaybe [] $ sequence $ filter (/= Nothing) $ map (\n ->
    let nw = wikipedia n in case filter (\s -> wikipedia s == nw) os of
        [] -> Nothing
        [o] -> Just (o, n)
    ) ns

compareStats :: (WikipediaStats, WikipediaStats) -> WikipediaStats
compareStats (os, ns) = WikipediaStats
    { wikipedia = wikipedia os
    , contentPages = Just $ diff (contentPages os) (contentPages ns)
    , pages = Just $ diff (pages os) (pages ns)
    , pageEdits = Just $ diff (pageEdits os) (pageEdits ns)
    , registeredUsers = Just $ diff (registeredUsers os) (registeredUsers ns)
    }
  where
    diff Nothing _ = 0
    diff _ Nothing = 0
    diff (Just o) (Just n) = n - o

compareStats' :: (WikipediaStats, WikipediaStats) -> WikipediaStats
compareStats' (os, ns) = WikipediaStats
    { wikipedia = wikipedia os
    , contentPages = Just $ round $ diff (contentPages os) (contentPages ns)
    , pages = Just $ round $ diff (pages os) (pages ns)
    , pageEdits = Just $ round $ diff (pageEdits os) (pageEdits ns)
    , registeredUsers = Just $ round $ diff (registeredUsers os) (registeredUsers ns)
    }
  where
    diff Nothing _ = 0
    diff _ Nothing = 0
    diff (Just o) (Just n) = logBase 10 (fromIntegral n) - logBase 10 (fromIntegral o)

-- TODO return a list
prettyComparedStats :: WikipediaStats -> String
prettyComparedStats (WikipediaStats _ (Just 0) (Just 0) (Just 0) (Just 0)) = ""
prettyComparedStats (WikipediaStats (Wikipedia _ lang) (Just cp) (Just p) (Just pe) (Just ru)) =
       if cp > 0 then "TODO" else ""
    ++ if p > 0 then "TODO" else ""
    ++ if pe > 0 then "TODO" else ""
    ++ if ru > 0 then "TODO" else ""


-- TODO config loading (verbosity, path to cache file)
-- TODO threshold passing detection: might be better to use a map of wiki to stats everywhere
-- TODO tweeting (limit to one tweet per period: the one belonging to the highest-ranking wiki)
-- TODO tests, https://hspec.github.io

-- https://docs.haskellstack.org/en/stable/README/
-- https://hackage.haskell.org/package/html-conduit
-- http://hackage.haskell.org/package/twitter-conduit

verbose :: Bool
verbose = True

statsPath :: FilePath
statsPath = "test.json"

testing :: IO ()
testing = do
    putStr "Loading previous stats... "
    oldS <- loadStats statsPath
    putStrLn "done."
    when verbose $ putStrLn $ show oldS

    putStr "Getting list of Wikipedias... "
    w <- getWikipedias
    w <- return $ take 10 w  -- TODO just for testing
    let m = length w
    putStrLn $ "got " ++ show m ++ "."
    when verbose $ putStrLn $ show w

    let numberedWikipedias = zip [0..] w
    s <- mapM (\(n, w) -> do
        putStr $ "Getting stats for " ++ subdomain w ++ ".wikipedia.org (" ++ show n ++ "/" ++ show m ++ ")... "
        s <- getStats w
        putStrLn $ "done."
        when verbose $ putStrLn $ show s
        return s
        ) numberedWikipedias

    case oldS of
        (Just os) -> do
            let pairs = pairUp os s
            let comp = map compareStats' pairs
            putStrLn $ show comp
            let pret = map prettyComparedStats comp
            putStrLn $ show pret
        Nothing -> return ()

    putStr "Storing updated stats... "
    storeStats statsPath s
    putStrLn "done."


{-# LANGUAGE OverloadedStrings #-}

module StatsIO where

import Types

import qualified Data.Text as T      (unpack, pack)
import qualified Text.JSON as JSON   (Result(..), decode, encode)
import           Network.HTTP.Simple (httpSink)
import           Network.HTTP.Client.Conduit (parseUrlThrow)
import           Text.HTML.DOM       (sinkDoc)
import           Text.XML.Cursor     (attributeIs, hasAttribute, child, content, element, fromDocument, ($//), (&/), (&//))
import           Text.XML            (Document)
import           Text.Read           (readMaybe)
import           Data.Char           (isDigit)
import           System.Directory    (doesFileExist)
import           Data.Maybe          (catMaybes)
import           Data.Map            (Map)
import qualified Data.Map as Map

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

getStatsPage :: Wikipedia -> IO Document
getStatsPage w = do
    let url = "https://" ++ subdomain w ++ ".wikipedia.org/wiki/Special:Statistics"
    req <- parseUrlThrow url
    httpSink req $ const sinkDoc

extractStat :: Document -> String -> IO (Maybe Integer)
extractStat doc c = do
    let cursor = fromDocument doc
    let stat = cursor
               $// attributeIs "class" (T.pack c)
               &// attributeIs "class" "mw-statistics-numbers"
               &/ content
    return $ sanitize stat
  where
    sanitize stat = readMaybe $ filter isDigit $ (T.unpack . head) stat :: Maybe Integer

getStats :: Wikipedia -> IO WikipediaStats
getStats w = do
    doc <- getStatsPage w
    let classes = Map.keys statDescriptions
    stats <- mapM (extractStat doc) classes
    let stats2 = map (\(c, s) -> case s of
                                    Nothing -> Nothing  -- TODO this can be made more elegant by using map/fmap?
                                    Just a -> Just (c, a)) $ zip classes stats
    return $ Map.fromList $ catMaybes stats2

readStats :: FilePath -> IO (Maybe WikipediasStats)
readStats f = do
    exists <- doesFileExist f
    if not exists
        then return Nothing
        else do
            contents <- readFile f
            let r = JSON.decode contents
            case r of
                (JSON.Error s) -> return Nothing
                (JSON.Ok s)    -> return $ Just s

writeStats :: FilePath -> WikipediasStats -> IO ()
writeStats f s = do
    writeFile f (JSON.encode s)

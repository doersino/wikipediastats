module Types where

import           Text.JSON           (JSON, Result(..), showJSON, readJSON)
import           Data.Map            (Map)
import qualified Data.Map as Map

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

type StatIdentifier = String

-- TODO note that this isn't a type, but whatever
statDescriptions :: Map StatIdentifier (String -> String)
statDescriptions = Map.fromList
    [ ("mw-statistics-articles", \s -> "now contains " ++ s ++ " articles")
    , ("mw-statistics-pages",    \s -> "now consists of " ++ s ++ " pages (including non-articles)")
    , ("mw-statistics-edits",    \s -> "has now received " ++ s ++ " edits")
    , ("mw-statistics-users",    \s -> "has now been shaped by " ++ s ++ " registered users")
    , ("mw-statistics-hook",     \s -> "now weighs in at " ++ s ++ " words")
    ]

type WikipediaStats = Map StatIdentifier Integer
type WikipediasStats = Map Wikipedia WikipediaStats

type WikipediasStatsCompared = WikipediasStats

module StatsProcessing where

import Types

import           Data.Maybe          (fromMaybe)
import           Data.Map            (Map)
import qualified Data.Map as Map

-- test: compareStats $ fromList [(Wikipedia {subdomain = "zu", language = "Zulu"}, (fromList [("mw-statistics-articles",4842),("mw-statistics-edits",91354),("mw-statistics-pages",8433),("mw-statistics-users",13375)],fromList [("mw-statistics-articles",5001),("mw-statistics-edits",91354),("mw-statistics-pages",8433),("mw-statistics-users",13375)]))]
-- TODO cleanup naming: os, ns, ov, nv
compareStats :: WikipediasStats -> WikipediasStats -> WikipediasStatsCompared
compareStats os ns = Map.intersectionWith (\o n ->
                            Map.filter (/= 0) $ Map.intersectionWith milestoneReached o n) os ns
  where
    milestoneReached ov nv = if firstDigitChanged ov nv
        then read $ (head $ show nv) : (take (length (tail $ show nv)) $ repeat '0')
        else 0
    firstDigitChanged ov nv = (head $ show nv) /= (head $ show ov)

-- TODO can make this function a bit more point-free
-- TODO rename, fix names within
prettyComparedStats :: WikipediasStatsCompared -> [String]
prettyComparedStats s = concatMap Map.elems $ Map.elems $ (flip Map.mapWithKey) s $
                            \w c -> (flip Map.mapWithKey) c $
                            \cl dif -> buildTweetText w cl dif

-- TODO convert zeros into natural language in some cases and sometimes insert spaces: 3 million, 300 000
buildTweetText :: Wikipedia -> StatIdentifier -> Integer -> String
-- TODO cl -> id?
buildTweetText w cl n = "The " ++ language w ++ " edition of Wikipedia "
                        ++ (fromMaybe (\_ -> "") $ Map.lookup cl statDescriptions) (formatStat n)
                        ++ "! Enjoy more stats here: https://" ++ subdomain w ++ ".wikipedia.org/wiki/Special:Statistics"

-- TODO tests
formatStat :: Integer -> String
formatStat n | n < 10000          = show n
             | n < 1000000        = let s = show n in
                                        take (length s - 3) s ++ "," ++ drop (length s - 3) s
             | n < 1000000000     = (show $ n `div` 1000000) ++ " million"
             | n < 1000000000000  = (show $ n `div` 1000000000) ++ " billion"
             | otherwise          = (show $ n `div` 1000000000000) ++ " trillion"

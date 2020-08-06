module StatsProcessing where

import Types

import           Data.Maybe          (fromMaybe)
import           Data.Map            (Map)
import qualified Data.Map as Map

associateStats :: [Wikipedia] -> [WikipediaStats] -> WikipediasStats
associateStats ws ss = Map.fromList $ zip ws ss

compareStats :: WikipediasStats -> WikipediasStats -> WikipediasStatsCompared
compareStats = Map.intersectionWith (\os ns -> Map.filter (/= 0) $ Map.intersectionWith milestoneReached os ns)
  where
    milestoneReached o n = if newEclipsesOld o n && aboveThreshold n && firstDigitChanged o n
        then read $ (head $ show n) : (take (length (tail $ show n)) $ repeat '0')
        else 0
    newEclipsesOld    o n = n > o    -- make sure the new stat is actually larger
    aboveThreshold      n = n >= 10  -- only tweet when a stat is >=10
    firstDigitChanged o n = (head $ show n) /= (head $ show o)

prettyComparedStats :: WikipediasStatsCompared -> [String]
prettyComparedStats sc = concatMap Map.elems $ Map.elems $ (flip Map.mapWithKey) sc $
                            \w s -> (flip Map.mapWithKey) s $
                            \id stat -> buildTweetText w id stat

buildTweetText :: Wikipedia -> StatIdentifier -> Integer -> String
buildTweetText w id stat = (if (head . show) stat == '1' then "Hooray! " else "")
                           ++ "The " ++ language w ++ " edition of #Wikipedia "
                           ++ (fromMaybe (\_ -> "") $ Map.lookup id statDescriptions) (formatStat stat)
                           ++ "! Explore more stats here: https://" ++ subdomain w ++ ".wikipedia.org/wiki/Special:Statistics"

formatStat :: Integer -> String
formatStat n | n < 10000          = show n
             | n < 1000000        = let s = show n
                                    in take (length s - 3) s ++ "," ++ drop (length s - 3) s
             | n < 1000000000     = (show $ n `div` 1000000) ++ " million"
             | n < 1000000000000  = (show $ n `div` 1000000000) ++ " billion"
             | otherwise          = (show $ n `div` 1000000000000) ++ " trillion"

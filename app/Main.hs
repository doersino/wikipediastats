import Types
import Config
import StatsIO
import StatsProcessing
import Tweeting

import qualified Data.Text as T      (unpack, pack)
import           Control.Monad       (when)
import qualified Data.Map as Map


-- TODO decent test coverage for data manipulation functions, use https://hspec.github.io
-- TODO error handling: https://stackoverflow.com/questions/6009384/exception-handling-in-haskell

-- TODO finally, check for stray TODOs

runWith :: Config -> IO ()
runWith c = do
    status "Successfully loaded configuration."
    debug $ show c

    status "Getting list of Wikipedias... "
    w <- getWikipedias
    w <- return $ take (limit $ generalConfig c) w
    let m = length w
    statusLn $ "got " ++ show m ++ "."
    debug $ show w

    let numberedWikipedias = zip [0..] w
    s2 <- (flip mapM) numberedWikipedias $ \(n, w) -> do
        status $ "Getting stats for " ++ subdomain w ++ ".wikipedia.org (" ++ show n ++ "/" ++ show m ++ ")... "
        s <- getStats w
        statusLn $ "done."
        debug $ show s
        return s
    let newStats = Map.fromList $ zip w s2  -- TODO move this to a function in StatsProcessing, or StatsIO

    status "Reading previous stats from cache... "
    oldStats' <- readStats $ cacheFile $ generalConfig c
    statusLn "done."
    debug $ show oldStats'

    case oldStats' of
        Just oldStats -> do
            status "Comparing newly downloaded stats with cached stats... "
            let comp = compareStats oldStats newStats
            debug $ show comp

            status "Turning comparison results into tweet texts... "
            let tweetTexts' = prettyComparedStats comp
            statusLn "done."
            mapM_ debug tweetTexts'

            tweetTexts <- if length tweetTexts' > 3
                then do
                    status "Only keeping the first three to avoid exceeding API limits (or the level of interest of followers)... "
                    let top3 = take 3 $ tweetTexts'
                    statusLn "done."
                    mapM_ debug top3
                    return top3
                else return tweetTexts'

            if (tweetingPossible $ twitterConfig c)
                then do
                    status "Tweeting... "
                    mapM_ (postTweet $ twitterConfig c) tweetTexts
                    statusLn "done."
                else do
                    statusLn "Tweeting is disabled since not all API keys and secrets have been specified."
                    return ()
        Nothing -> do
            statusLn "Since no cached stats were present, I've got nothing to compare them against. Run me again and I will."
            return ()

    status "Writing updated stats to cache... "
    writeStats (cacheFile $ generalConfig c) newStats
    statusLn "done."
  where
    status s   = when (verbosity (generalConfig c) > 0) $ putStr s
    statusLn s = status $ s ++ "\n"
    debug  s   = when (verbosity (generalConfig c) == 2) $ putStrLn s

main :: IO ()
main = do
    configFile <- readFile "config.ini"
    runWith $ parseIni configFile

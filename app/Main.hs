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

-- TODO make output helper methods, maybe somehow with reader monad for config?

-- TODO finally, check for stray TODOs

main :: IO ()
main = do
    configFile <- readFile "config.ini"
    let config = parseIni configFile
    let generalConf = generalConfig config
    let twitterConf = twitterConfig config
    let cachePath = cacheFile generalConf
    let notSilent = verbosity generalConf > 0
    let verbose   = verbosity generalConf == 2
    when notSilent $ putStrLn "Successfully parsed configuration."
    when verbose $ putStrLn $ show config

    when notSilent $ putStr "Loading previous stats from cache... "
    oldS <- readStats cachePath
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
    let s = Map.fromList $ zip w s2  -- TODO move this to a function in StatsProcessing, or StatsIO

    case oldS of
        Just os -> do
            when notSilent $ putStr "Comparing newly downloaded stats with cached stats... "
            let comp = compareStats os s
            when verbose $ putStrLn $ show comp

            when notSilent $ putStr "Turning comparison results into tweets... "
            let pret = prettyComparedStats comp
            when verbose $ mapM_ putStrLn pret
            when notSilent $ putStrLn "done."

            pret2 <- if length pret > 3
                     then do
                        when notSilent $ putStr "Only keeping the first three to avoid exceeding API limits... "
                        let top3 = take 3 $ pret
                        when verbose $ mapM_ putStrLn pret
                        when notSilent $ putStrLn "done."
                        return top3
                     else return pret

            if tweetingPossible twitterConf
                then do
                    when notSilent $ putStr "Tweeting... "
                    mapM_ (postTweet twitterConf) pret2
                    when notSilent $ putStrLn "done."
                else do
                    when notSilent $ putStrLn "Tweeting is disabled since not all API keys and secrets have been specified."
                    return ()
        Nothing -> do
            when notSilent $ putStrLn "Since no cached stats were present, I've got nothing to compare them against. Run me again and I will."
            return ()

    when notSilent $ putStr "Storing updated stats... "
    writeStats cachePath s
    when notSilent $ putStrLn "done."

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Types
import Config
import StatsIO
import StatsProcessing
import Tweeting

import qualified Data.Map as Map

main :: IO ()
main = hspec $ do
    --describe "Prelude.head" $ do
    --    it "returns the first element of a list" $ do
    --        head [23 ..] `shouldBe` (23 :: Int)
    --    it "returns the first element of an *arbitrary* list" $
    --        property $ \x xs -> head (x:xs) == (x :: Int)
    --    it "throws an exception if used with an empty list" $ do
    --        evaluate (head []) `shouldThrow` anyException

    describe "tweetingPossible" $ do
        it "tweeting impossible if none of the api credentials are set" $ do
            let tc = TwitterConfig "" "" "" ""
            tweetingPossible tc `shouldBe` False
        it "tweeting impossible if none of the api credentials are set" $ do
            let tc = TwitterConfig "abc" "" "" "abc"
            tweetingPossible tc `shouldBe` False
        it "tweeting possible if all of the api credentials are set" $ do
            let tc = TwitterConfig "abc" "abc" "abc" "abc"
            tweetingPossible tc `shouldBe` True

    describe "formatStat" $ do
        it "100" $ do
            formatStat 100 `shouldBe` "100"
        it "1000" $ do
            formatStat 1000 `shouldBe` "1000"
        it "9999" $ do
            formatStat 9999 `shouldBe` "9999"
        it "10,000" $ do
            formatStat 10000 `shouldBe` "10,000"
        it "52,503" $ do
            formatStat 52503 `shouldBe` "52,503"
        it "999,999" $ do
            formatStat 999999 `shouldBe` "999,999"
        it "1 million" $ do
            formatStat 1000000 `shouldBe` "1 million"
        it "9 million" $ do
            formatStat 9000240 `shouldBe` "9 million"
        it "5 billion" $ do
            formatStat 5000000000 `shouldBe` "5 billion"
        it "2 trillion" $ do
            formatStat 2004300000000 `shouldBe` "2 trillion"

    let w1 = Wikipedia {subdomain = "aa", language = "Afar"}
    let w2 = Wikipedia {subdomain = "ab", language = "Abkhazian"}
    let w3 = Wikipedia {subdomain = "ace", language = "Acehnese"}

    let ws1 = Map.fromList [ ("mw-statistics-articles", 0)
                           , ("mw-statistics-edits",    500000)
                           , ("mw-statistics-hook",     99)
                           , ("mw-statistics-pages",    7)
                           , ("mw-statistics-users",    10000000)
                           ]
    let ws2 = Map.fromList [ ("mw-statistics-articles", 0)
                           , ("mw-statistics-edits",    500000)
                           , ("mw-statistics-hook",     999)
                           , ("mw-statistics-pages",    8)
                           , ("mw-statistics-users",    10000001)
                           ]
    let ws3 = Map.fromList [ ("mw-statistics-articles", 0)
                           , ("mw-statistics-edits",    700000)
                           , ("mw-statistics-hook",     9999)
                           , ("mw-statistics-pages",    9)
                           , ("mw-statistics-users",    40)
                           ]
    let ws4 = Map.fromList [ ("mw-statistics-articles", 1)
                           , ("mw-statistics-edits",    1000000)
                           , ("mw-statistics-hook",     10000)
                           , ("mw-statistics-pages",    10)
                           , ("mw-statistics-users",    1000000001)
                           ]

    let wss1 = Map.fromList [(w1, ws1), (w2, ws3)]
    let wss2 = Map.fromList [(w1, ws2), (w2, ws4)]
    let wss3 = Map.fromList [(w1, ws3), (w2, ws4), (w3, ws1)]
    let wss4 = Map.fromList [(w1, ws4), (w2, ws4), (w3, ws1)]

    let cmp1 = Map.fromList [ (w1, Map.empty)
                            , (w2, Map.fromList [ ("mw-statistics-edits",    1000000)
                                                , ("mw-statistics-hook",     10000)
                                                , ("mw-statistics-pages",    10)
                                                , ("mw-statistics-users",    1000000000)
                                                ])
                            ]
    let cmp2 = Map.fromList [ (w1, Map.fromList [("mw-statistics-edits", 700000)])
                            , (w2, Map.empty)
                            ]
    let cmp3 = Map.fromList [ (w1, Map.fromList [ ("mw-statistics-edits",    1000000)
                                                , ("mw-statistics-hook",     10000)
                                                , ("mw-statistics-pages",    10)
                                                , ("mw-statistics-users",    1000000000)
                                                ])
                            , (w2, Map.empty)
                            , (w3, Map.empty)
                            ]
    let cmp4 = Map.fromList [ (w1, Map.empty)
                            , (w2, Map.empty)
                            ]

    describe "associateStats" $ do
        it "[w1, w2] & [ws1, ws3] = wss1" $ do
            associateStats [w1, w2] [ws1, ws3] `shouldBe` wss1
        it "[w1, w2, w3] & [ws1, ws3, ws1] = wss3" $ do
            associateStats [w1, w2, w3] [ws3, ws4, ws1] `shouldBe` wss3

    describe "compareStats" $ do
        it "wss1 & wss2 = cmp1" $ do
            compareStats wss1 wss2 `shouldBe` cmp1
        it "wss2 & wss3 = cmp2" $ do
            compareStats wss2 wss3 `shouldBe` cmp2
        it "wss3 & wss4 = cmp3" $ do
            compareStats wss3 wss4 `shouldBe` cmp3
        it "wss4 & wss1 = cmp4" $ do
            compareStats wss4 wss1 `shouldBe` cmp4

    describe "buildTweetText" $ do
        it "afar wikipedia has reached 1000 articles" $ do
            buildTweetText w1 "mw-statistics-articles" 1000
                `shouldBe` "The Afar edition of #Wikipedia now contains 1000 articles! Enjoy more stats here: https://aa.wikipedia.org/wiki/Special:Statistics"
        it "afar wikipedia has reached 50,000 edits" $ do
            buildTweetText w1 "mw-statistics-edits" 50000
                `shouldBe` "The Afar edition of #Wikipedia has now received 50,000 edits! Enjoy more stats here: https://aa.wikipedia.org/wiki/Special:Statistics"
        it "afar wikipedia has reached a million words" $ do
            buildTweetText w1 "mw-statistics-hook" 1000000
                `shouldBe` "The Afar edition of #Wikipedia now weighs in at 1 million words! Enjoy more stats here: https://aa.wikipedia.org/wiki/Special:Statistics"

    describe "prettyComparedStats" $ do
        it "cmp2 results in one line" $ do
            prettyComparedStats cmp2
                `shouldBe` ["The Afar edition of #Wikipedia has now received 700,000 edits! Enjoy more stats here: https://aa.wikipedia.org/wiki/Special:Statistics"]
        it "cmp4 is empty" $ do
            prettyComparedStats cmp4 `shouldBe` []

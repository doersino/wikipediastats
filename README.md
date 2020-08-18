# wikipediastats

*A Haskell-powered Twitter bot that posts milestones and statistcs of various Wikipedias.*

While the main purpose of building this Twitter bot was to get myself reacquainted with Haskell, it's actually doing semi-interesting stuff. Whenever you run this program, it

1. downloads and parses [a list of all the different-language Wikipedias](https://meta.wikimedia.org/wiki/List_of_Wikipedias),
2. **scrapes some of the more interesting [statistics](https://en.wikipedia.org/wiki/Special:Statistics)** for each of them,
3. **compares these stats to previously scraped and cached values** (unless the cache doesn't exist, in which case `goto 5`),
4. **[fires off](https://github.com/himura/twitter-conduit/blob/master/sample/post.hs) a tweet if a milestone has been reached**, i.e. the first digit of a stat has changed (e.g. 49894 → 50002), and
5. refreshes the now-stale cache with the newly scraped values.


#### Now witness the ~~firepower~~results of this fully armed and operational ~~battle station~~Twitter bot and check out [@wikipediastats](https://twitter.com/wikipediastats)!

*(As it turns out, the otherwise-[excellent](https://uberspace.de) shared hosting plan I'm running all of my Twitter bots on limits RAM use to 1.5 GB per user, which is insufficient for building some of the dependencies of this bot. As a result, I've decided to reimplement and properly deploy the bot in Node.js – another language that I'd like to get up to speed on. A link will be forthcoming, but as a result, this repository will likely be unmaintained.)*


## Setup

Fairly typical for a modern Haskell thing, I believe. First, install a reasonably recent release of [Stack](https://docs.haskellstack.org/en/stable/README/). Then:

```bash
$ git clone https://github.com/doersino/wikipediastats
$ cd wikipediastats
$ stack build
```

If that's been successful, make a copy of `config.sample.ini`, name it `config.ini` and fill in your Twitter API credentials as described in the helpful comment you'll find in there.

Run the bot at least once during setup to build the initial cache:

```bash
$ stack run
```

Optionally, you can play around a bit:

```bash
$ stack repl
```

Or verify that everything's working just swell:

```bash
$ stack test
```

If you're actually intending to use this as a Twitter bot, set up a cronjob to execute `stack run` every hour or so, roughly like this:

```cron
0 * * * * cd PATH_TO_WIKIPEDIASTATS; stack run
```


## Notes

* This three-afternoon project was my first foray into Haskell after not touching it for a couple of years (and back then, I was firmly lodged in the beginner-to-intermediate gap). Don't expect elegance, custom monads or adherence to best practices.
* I haven't bothered listing version ranges for the dependencies of this tool in `package.yaml` because I don't know which past or future versions will invariably break things, but I think the fixed Stackage resolver version makes this less problematic than it used to be before Stack was around? Not sure. If you, a future software historian, can't seem to get the dependecies to play along nicely, I'm quite sorry.
* An improvement I didn't care to implement: Store the largest tweeted value (for each stat, for each Wikipedia) in the cache in order to avoid duplicate tweets when the stat reaches a milestone, falls below it again due to article deletions or similar, then reaches the milestone again.
* Some Wikipedias are ignored by this bot – that's those whose statistics pages don't use Arabic numerals. This could probably be worked around in various ways, but I'm too white to consider it a priority for something as inconsequential as a Twitter bot. But if my instance of this bot attains more than an handful of followers, I'll make sure to revisit this issue.

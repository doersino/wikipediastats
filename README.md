# wikipediastats

While the main purpose of building this Twitter bot was to get myself reacquainted with Haskell, it's actually doing semi-interesting stuff. Whenever you run the executable, it

1. downloads and parses [a list of all the different-language Wikipedias](https://meta.wikimedia.org/wiki/List_of_Wikipedias),
2. **scrapes some of the more interesting [statistics](https://en.wikipedia.org/wiki/Special:Statistics)** for each of them,
3. **compares these stats to previously scraped and cached values** (unless the cache doesn't exist, in which case `goto 5`),
4. **[fires off](http://hackage.haskell.org/package/twitter-conduit) a tweet if a milestone has been reached**, i.e. the first digit of a stat has changed (e.g. 49894 → 50002), and
5. refreshes the now-stale cache with the newly scraped values.


#### Now witness the ~~firepower~~results of this fully armed and operational ~~battle station~~Twitter bot by checking out [@wikipediastats](https://twitter.com/wikipediastats)!


## Setup

Fairly typical for a modern Haskell thing, I believe. First, install a reasonably recent release of [Stack](https://docs.haskellstack.org/en/stable/README/). Then:

```
git clone https://github.com/doersino/wikipediastats
cd wikipediastats
stack build
```

Then, make a copy of `config.sample.ini`, name it `config.ini` and fill in your Twitter API credentials as described in the helpful comment you'll find in there.

Optionally, you can play around a bit:

```
stack repl
```

Or verify that everything's working just swell:

```
stack test
```

If you're actually intending to use this as a Twitter bot, set up a cronjob to execute `stack run` every hour or so, roughly like this:

TODO check!

```
0 * * * * cd PATH_TO_WIKIPEDIASTATS; stack run
```


## Notes

* This three-afternoon project was my first foray into Haskell after not using it for a couple of years (and back then, I was firmly lodged in the beginner-to-intermediate gap). Don't expect elegance, custom monads or adherence to best practices.
* I haven't bothered listing version ranges for the dependencies of this tool in `package.yaml` because I don't know which past or future versions will invariably break things, but I think the choice of stackage resolver makes this less problematic than it used to be before Stack was around? Not sure. If you, a future software historian, can't seem to get the dependecies to play along nicely, I'm sorry.

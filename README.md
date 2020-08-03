# wikipediastats

TODO

## Setup

Install Stack. Then

TODO stack version? set more concrete version ranges in package.yaml too

```
git clone https://github.com/doersino/wikipediastats
cd wikipediastats
vi config.ini  # fill in twitter credentials, modify other settings
stack build
```

Set up a cronjob to execute `stack run` every hour or so.

## Playing around, testing

```
stack repl
stack test
```

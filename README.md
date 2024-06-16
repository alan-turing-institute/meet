# Meet people around the Turing

## Installation

```shell
brew tap alan-turing-institute/hut23
brew install alan-turing-institute/hut23/meet
```

## From source?

[Install `ghcup`](https://www.haskell.org/ghcup/), then use it to install GHC 9.4 and Cabal 3.8.
(Newer versions of GHC and Cabal should in theory also work.)

```
git clone git@github.com:alan-turing-institute/meet.git
cd meet
cabal install
```

## Usage

This package provides two executables: **`meet`** and **`meet-rooms`**.
The former is used to find a meeting time for a group of people (and associated meeting rooms), whereas the latter just shows room availabilility.

You can use the `-h` flag to get help on how to use each of these executables:

### meet

```shell
> meet -h

meet - a tool to schedule a meeting

Usage: meet EMAILS... [-i|--interval MINUTES] [-d|--duration MINUTES] 
            [-s|--startDate YYYY-MM-DD] [-t|--timespan DAYS] 
            [-p|--people PEOPLE] [-l|--lucky]

  Schedule a meeting with the given emails.

Available options:
  EMAILS...                Email addresses of the people you want to stalk. If
                           you don't include @turing.ac.uk, it will be appended
                           for you.
  -i,--interval MINUTES    Granularity of schedule fetched. Defaults to 15
                           minutes.
  -d,--duration MINUTES    Duration of the meeting. Defaults to 60 minutes.
  -s,--startDate YYYY-MM-DD
                           First day to start searching for a meeting on.
  -t,--timespan DAYS       Number of days to look ahead when searching for
                           meeting slots. Defaults to a week.
  -p,--people PEOPLE       Number of people who will be attending in person.
  -l,--lucky               Make the app suggest a single best meeting time (and
                           room if needed).
  -h,--help                Show this help text
```

### meet-rooms

```shell
> meet-rooms -h

meet-rooms - find a meeting room for a pre-existing meeting

Usage: meet-rooms [-s|--startDate YYYY-MM-DD] [-t|--timespan DAYS] 
                  [-c|--capacity PEOPLE]

  Find a meeting room

Available options:
  -s,--startDate YYYY-MM-DD
                           First day to start searching for a meeting room on.
  -t,--timespan DAYS       Number of days to look ahead when searching for
                           meeting slots. Defaults to 1, i.e., search only on
                           the start date.
  -c,--capacity PEOPLE     Minimum capacity needed for the meeting room.
                           Defaults to 0.
  -h,--help                Show this help text
```

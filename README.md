# Meet people around the Turing

## Installation

```shell
brew tap alan-turing-institute/hut23
brew install alan-turing-institute/hut23/meet
```

## From source?

[Install `ghcup`](https://www.haskell.org/ghcup/), then use it to install GHC 9.4 and Cabal 3.8.
(Other GHC/Cabal version combinations that are known to work are 9.6/3.8 and 9.8/3.10.
You can also check the version combinations we test [in GitHub Actions](https://github.com/alan-turing-institute/meet/blob/main/.github/workflows/build.yml).)

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
            [-p|--people PEOPLE] [-l|--lucky] [--local]

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
  --local                  Display meeting times in your local timezone. By
                           default, times are shown in London time.
  -h,--help                Show this help text
```

### meet-rooms

```shell
> meet-rooms -h

meet-rooms - find a meeting room for a pre-existing meeting

Usage: meet-rooms [-s|--startDate YYYY-MM-DD] [-t|--timespan DAYS]
                  [-c|--capacity PEOPLE] [--local]

  Find a meeting room

Available options:
  -s,--startDate YYYY-MM-DD
                           First day to start searching for a meeting room on.
  -t,--timespan DAYS       Number of days to look ahead when searching for
                           meeting slots. Defaults to 1, i.e., search only on
                           the start date.
  -c,--capacity PEOPLE     Minimum capacity needed for the meeting room.
                           Defaults to 0.
  --local                  Display meeting times in your local timezone. By
                           default, times are shown in London time.
  -h,--help                Show this help text
```

### Updating Homebrew tap

1. Increment version number in `meet.cabal`. Commit to the main branch. (There isn't a hard and fast rule for whether to bump the major/minor/patch versions, use your judgment as to whether something is breaking.)
2. Run `git tag -a v0.x.y.z` then `git push --tags`
3. Get the full commit SHA corresponding to the tag. Then edit line 4 of the `meet.rb` in the `homebrew-hut23` repository to include both the new version and the SHA: https://github.com/alan-turing-institute/homebrew-hut23/blob/main/meet.rb
4. Build the bottle with `brew update; brew uninstall meet; brew install meet --build-bottle; brew bottle meet --no-rebuild`
5. Edit the name of the bottle file, replace `meet--0.x.y.z` with `meet-0.x.y.z` (remove the extra hyphen)
6. Create a new GitHub release on `meet` for the new version
7. Upload the renamed bottle file as a binary associated with the release
8. Inside the `bottle do` block of the `meet.rb` file, replace the `sha256 cellar`... lines with what Homebrew output. Update the version number in the `root_url...` line.
9. You can duplicate the `sha256 cellar` line and replace `arm64_ventura` with `arm64_monterey` so that people with macOS Monterey can also use it (the binary will work for them). If you do this, you will need to copy the bottle file, rename it to `...monterey...` and upload it to the release as well.
10. Test that it works with `brew update; brew reinstall meet`

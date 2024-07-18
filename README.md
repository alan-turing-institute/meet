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

You can use the `-h` flag to get help on how to use each of these executables and view the complete options, but basic usage looks something like:

`meet aturing jdoe -s 2024-01-01 -d 30` — find a time for `aturing` and `jdoe` to meet on New Year's Day for half an hour

`meet-rooms -s 2024-01-01` — list meeting room availability on New Year's Day


### Developer notes: updating Homebrew tap

1. Increment version number in `meet.cabal`. Commit to the main branch. (There isn't a hard and fast rule for whether to bump the major/minor/patch versions, use your judgment as to whether something is breaking.)
2. Run `git tag -a v0.x.y.z` then `git push --tags`
3. Get the full commit SHA corresponding to the tag. Then edit line 4 of the `meet.rb` in the `homebrew-hut23` repository to include both the new version and the SHA: https://github.com/alan-turing-institute/homebrew-hut23/blob/main/meet.rb
4. Build the bottle with `brew update; brew uninstall meet; brew install meet --build-bottle; brew bottle meet --no-rebuild`
5. Edit the name of the bottle file, replace `meet--0.x.y.z` with `meet-0.x.y.z` (remove the extra hyphen)
6. Create a new GitHub release on `meet` for the new version
7. Upload the renamed bottle file as a binary associated with the release
8. Inside the `bottle do` block of the `meet.rb` file, replace the `sha256 cellar`... lines with what Homebrew output. Update the version number in the `root_url...` line.
9. You can duplicate the `sha256 cellar` line and replace `arm64_ventura` with `arm64_monterey` so that people with macOS Monterey can also use it (the binary will work for them). If you do this, you will need to copy the bottle file, rename it to `...monterey...` and upload it to the release as well.
10. Test that it works with `brew update; brew uninstall meet; brew install meet`. (Using `brew reinstall meet` for some reason makes Homebrew attempt to reinstall from source, possibly because the existing installation is from source.)

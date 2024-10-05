# Meet people around the Turing

<p align="center"><img src="https://private-user-images.githubusercontent.com/122629585/373836150-fa9cbc9f-98cb-49db-9cff-9c8a3ac47651.gif?jwt=eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJnaXRodWIuY29tIiwiYXVkIjoicmF3LmdpdGh1YnVzZXJjb250ZW50LmNvbSIsImtleSI6ImtleTUiLCJleHAiOjE3MjgwOTMzNDAsIm5iZiI6MTcyODA5MzA0MCwicGF0aCI6Ii8xMjI2Mjk1ODUvMzczODM2MTUwLWZhOWNiYzlmLTk4Y2ItNDlkYi05Y2ZmLTljOGEzYWM0NzY1MS5naWY_WC1BbXotQWxnb3JpdGhtPUFXUzQtSE1BQy1TSEEyNTYmWC1BbXotQ3JlZGVudGlhbD1BS0lBVkNPRFlMU0E1M1BRSzRaQSUyRjIwMjQxMDA1JTJGdXMtZWFzdC0xJTJGczMlMkZhd3M0X3JlcXVlc3QmWC1BbXotRGF0ZT0yMDI0MTAwNVQwMTUwNDBaJlgtQW16LUV4cGlyZXM9MzAwJlgtQW16LVNpZ25hdHVyZT0yZTIzOTJlN2M3ZjYxM2YyMzQwZThmOTk4ZGMwZGEyZmE3YThlNGVjNzI0NWJhZWJiNDUxMGNiNGFkNmRjYjBjJlgtQW16LVNpZ25lZEhlYWRlcnM9aG9zdCJ9.3EEfTaukT2V529SSvpGEguBcMiyKVXyqMNzPNby3BJg" alt="Demo usage of meet" width="600" /></p>

## Installation (macOS)

```shell
brew tap alan-turing-institute/hut23
brew install alan-turing-institute/hut23/meet
```

## Usage

This package provides two executables: **`meet`** and **`meet-rooms`**.
The former is used to find a meeting time for a group of people (and associated meeting rooms), whereas the latter just shows room availabilility.

You can use the `-h` flag to get help on how to use each of these executables and view the complete options, but basic usage looks something like:

- `meet aturing jdoe -s 2024-12-25 -d 30`

  Find a time for `aturing@turing.ac.uk` and `jdoe@turing.ac.uk` to meet on Christmas Day for half an hour.

- `meet-rooms -s 2024-12-25`

  List meeting room availability on Christmas Day.


--------

## Developer notes

### Compiling from source

[Install `ghcup`](https://www.haskell.org/ghcup/), then use it to install GHC 9.4 and Cabal 3.8.
(Other GHC/Cabal version combinations that are known to work are 9.6/3.8 and 9.8/3.10.
You can also check the version combinations we test [in GitHub Actions](https://github.com/alan-turing-institute/meet/blob/main/.github/workflows/build.yml).)

Then run:

```
git clone git@github.com:alan-turing-institute/meet.git
cd meet
cabal update
cabal build
```

If it builds successfully, you can then run e.g.

```
cabal run meet -- aturing jdoe -s 2024-12-25 -d 30
```

or 

```
cabal run meet-rooms -- -s 2024-12-25
```

### Updating the Homebrew tap

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

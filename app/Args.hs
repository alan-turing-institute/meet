module Args (parseArgs) where

data Args = Args
  { emails :: [Text],
    interval :: Int,
    duration :: Int
  }

parseArgs = undefined

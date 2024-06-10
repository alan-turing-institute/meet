module Main where

import Azure (getPerson)
import Data.Maybe (catMaybes)
import Types

chooseMeeting :: [Person] -> Meeting
chooseMeeting = undefined

main :: IO ()
main = do
  let emails = ["me@turing.ac.uk", "you@turing.ac.uk", "etc@turing.ac.uk"]
  maybePeople <- mapM getPerson emails
  let actualPeople = catMaybes maybePeople
  let meeting = chooseMeeting actualPeople
  print meeting

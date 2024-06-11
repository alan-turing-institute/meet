module Meetings where

import Types

data Entity = ERoom Int String | EPerson String

email :: Entity -> String
email (ERoom _ s) = s
email (EPerson s) = s

findMeetingTimes :: [Entity] -> [Meeting]
findMeetingTimes = undefined

-- do a find meeting request with all of the pre-defined meeting rooms
findMeetingRoom :: [Meeting] -> Int -> [Meeting]
findMeetingRoom = undefined

import Types


data Entity = Room Int String | Person String
email :: Entity -> String
email (Room _ s) = s
email (Person s) = s


parsePeople :: [(String, [Integer])] -> [Person]
parsePeople
parsePeople (x : xs) = Person x

findMeetingTimes :: [Entity] -> [Meeting]
findMeetingTimes = undefined

findMeetingRoom :: [Meeting] -> numPeople -> [Meeting]
-- do a find meeting request with all of the pre-defined meeting rooms
findMeetingRoom

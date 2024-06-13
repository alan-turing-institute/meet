Meet people around the Turing.

```
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
                           First day to start searching for a meeting on
  -t,--timespan DAYS       Number of days to look ahead when searching for
                           meeting slots
  -p,--people PEOPLE       Number of people who will be attending in person
  -l,--lucky               Make the app suggest a single best meeting time (and
                           room if needed)
  -h,--help                Show this help text
```

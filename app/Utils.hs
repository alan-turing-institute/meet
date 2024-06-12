module Utils (gracefulDivide) where

gracefulDivide :: Int -> Int -> IO Int
gracefulDivide numerator denominator = do
  let res = quot numerator denominator
  if (res==0)
    then error ("Meeting duration of " ++ show numerator ++ " minutes is shorter than the meeting interval of " ++ show denominator ++ " minutes.")
    else case (rem numerator denominator) of
      0 -> pure res
      r -> putStrLn ("WARNING: Meeting duration of " ++ show numerator ++ " minutes divides by " ++ show interval ++ " with " ++ show r ++ " minutes remaining, so we will book a " ++ (res * denominator) " minute meeting.") >> pure res

module Test.Framework.Location where


type Location = (String, Int)

showLoc :: Location -> String
showLoc (f,n) = f ++ ":" ++ show n


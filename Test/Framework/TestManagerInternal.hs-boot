module Test.Framework.TestManagerInternal where

data TestResult = Error | Fail | Pending | Pass
instance Read TestResult
instance Show TestResult
instance Eq TestResult

extractPendingMessage :: String -> Maybe String

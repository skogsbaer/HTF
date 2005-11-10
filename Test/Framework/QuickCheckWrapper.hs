module Test.Framework.QuickCheckWrapper (

  Id, Config(..), makeVerbose,

  testableAsAssertion,
 
  module Test.QuickCheck
) where

import qualified Data.Map as Map
import Control.Concurrent.MVar
import Control.Exception ( throw )
import System.IO
import System.IO.Unsafe
import System.Random
import Data.List( group, sort, intersperse )
import Data.Char
import Test.QuickCheck hiding ( Config(..), defaultConfig,
                                test, quickCheck, verboseCheck, check )
import Test.QuickCheck.Batch
import Test.Framework.HUnitWrapper

type Id = String

data Config = Config
  { configMaxTest :: Int
  , configMaxFail :: Int
  , configSize    :: Int -> Int
  , configEvery   :: String -> IO ()
  }

data QCState = QCState { qc_config :: Config }
             
defaultConfig =  Config
                 { configMaxTest = 100
                 , configMaxFail = 1000
                 , configSize    = (+ 3) . (`div` 2)
                 , configEvery   = \_ -> return ()
                 }

verboseConfigEvery = hPutStr stderr

makeVerbose :: Config -> Config
makeVerbose cfg = cfg { configEvery = verboseConfigEvery }

qcState :: MVar QCState
qcState = unsafePerformIO (newMVar (QCState defaultConfig))

testableAsAssertion :: Testable a => Id -> (Config -> Config, a) -> Assertion
testableAsAssertion id (f, t) = 
    withMVar qcState $ \state ->
        do let cfg = f (qc_config state)
           configEvery cfg (prop id ++ "\n")
           res <- check cfg t
           case res of
             PropOk s -> do hPutStrLn stderr $ " * " ++ prop id  ++ (strip s)
                            hPutStrLn stderr ""
             PropFailure s -> assertFailure $ prop id ++ (strip s)
             PropExhausted s -> assertFailure $ prop id ++ (strip s)
           return ()
    where prop s = "Property `" ++ s ++ "' "
          strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

data PropResult = PropOk String
                | PropFailure String
                | PropExhausted String
         
check :: Testable a => Config -> a -> IO PropResult
check config a =
  do rnd <- newStdGen
     tests config (evaluate a) rnd 0 0 []

tests :: Config -> Gen Result -> StdGen 
      -> Int -> Int -> [[String]] -> IO PropResult
tests config gen rnd0 ntest nfail stamps
  | ntest == configMaxTest config = 
      return $ done PropOk "OK, passed" ntest stamps
  | nfail == configMaxFail config = 
      return $ done PropExhausted "Arguments exhausted after" ntest stamps
  | otherwise               =
      do configEvery config $ show ntest ++ ":\n" ++ unlines (arguments result)
         case ok result of
           Nothing    ->
             tests config gen rnd1 ntest (nfail+1) stamps
           Just True  ->
             tests config gen rnd1 (ntest+1) nfail (stamp result:stamps)
           Just False ->
             return $ PropFailure  ("Falsifiable, after "
                                    ++ show ntest
                                    ++ " tests:\n"
                                    ++ unlines (arguments result)
                                   )
     where
      result      = generate (configSize config ntest) rnd2 gen
      (rnd1,rnd2) = split rnd0

done :: (String -> PropResult) -> String -> Int -> [[String]] -> PropResult
done f mesg ntest stamps =
 f ( mesg ++ " " ++ show ntest ++ " tests" ++ table )
 where
  table = display
        . map entry
        . reverse
        . sort
        . map pairLength
        . group
        . sort
        . filter (not . null)
        $ stamps

  display []  = ".\n"
  display [x] = " (" ++ x ++ ").\n"
  display xs  = ".\n" ++ unlines (map (++ ".") xs)

  pairLength xss@(xs:_) = (length xss, xs)
  entry (n, xs)         = percentage n ntest
                       ++ " "
                       ++ concat (intersperse ", " xs)

  percentage n m        = show ((100 * n) `div` m) ++ "%"
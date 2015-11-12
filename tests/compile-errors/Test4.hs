{-# OPTIONS_GHC -F -pgmF ../../scripts/local-htfpp #-}

import "HTF" Test.Framework
data D = D
-- error must be in line 7
prop_foo :: D -> Bool
prop_foo _ = False

main :: IO ()
main = return ()

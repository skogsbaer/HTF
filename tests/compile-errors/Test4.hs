{-# OPTIONS_GHC -F -pgmF ../../dist/build/htfpp/htfpp #-}

import Test.Framework
data D = D
-- error must be in line 7
prop_foo :: D -> Bool
prop_foo _ = False

main :: IO ()
main = return ()

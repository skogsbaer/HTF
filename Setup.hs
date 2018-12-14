#!/usr/bin/runhaskell

import Distribution.Simple
import System.Process

main = defaultMainWithHooks (simpleUserHooks { preConf = preTestHook })
    where
      preTestHook args flags =
          do system ("bash ./scripts/prepare")
             preConf simpleUserHooks args flags

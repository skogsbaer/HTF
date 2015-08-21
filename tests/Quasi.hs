{-# LANGUAGE TemplateHaskell #-}
module Quasi where

import Language.Haskell.TH.Quote
import GHC.Exts (IsString(..))

q :: QuasiQuoter
q = QuasiQuoter exprQ undefined undefined undefined
    where
      exprQ =
          (\a -> [|fromString a|]) . filter (/= '\r')

{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

{- |

This module defines the 'Pretty' type class. The assert functions
from 'Test.Framework.HUnitWrapper' use the pretty-printing functionality
provided by this type class so as to provide nicely formatted
error messages.

Additionally, this module re-exports the standard Haskell pretty-printing module
'Text.PrettyPrint'
-}
module Test.Framework.Pretty (

  Pretty(..), (<=>),

  module Text.PrettyPrint
)

where

import Text.PrettyPrint

-- | A type class for pretty-printable things.
-- Minimal complete definition: @pretty@.
class Pretty a where
    -- | Pretty-print a single value.
    pretty :: a -> Doc
    -- | Pretty-print a list of things.
    prettyList :: [a] -> Doc
    prettyList l =
        char '[' <> vcat (punctuate comma (map pretty l)) <> char ']'
    -- | Pretty-print a single value as a 'String'.
    showPretty :: a -> String
    showPretty = render . pretty

{-
instance Pretty String where
    pretty = text
-}

instance Pretty Char where
    pretty = char
    prettyList s = text s

instance Pretty a => Pretty [a] where
    pretty = prettyList

instance Pretty Int where
    pretty = int

instance Pretty Bool where
    pretty = text . show

-- | Utility function for inserting a @=@ between two 'Doc' values.
(<=>) :: Doc -> Doc -> Doc
d1 <=> d2 = d1 <+> equals <+> d2

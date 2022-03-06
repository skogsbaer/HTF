{-# LANGUAGE CPP, FlexibleInstances, TypeSynonymInstances #-}
--
-- Copyright (c) 2005-2022   Stefan Wehr - http://www.stefanwehr.de
--
-- This library is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Lesser General Public
-- License as published by the Free Software Foundation; either
-- version 2.1 of the License, or (at your option) any later version.
--
-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public
-- License along with this library; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA
--

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

#if MIN_VERSION_base(4,11,0)
-- Text.PrettyPrint exports (<>) conflicting with newer Prelude.
import Text.PrettyPrint hiding ((<>))
#else
import Text.PrettyPrint
#endif

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

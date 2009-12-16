{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Test.Framework.Pretty (

  Pretty(..), (<=>),

  module Text.PrettyPrint
)

where

import Text.PrettyPrint

class Pretty a where
    pretty :: a -> Doc
    prettyList :: [a] -> Doc
    prettyList l =
        char '[' <> vcat (punctuate comma (map pretty l)) <> char ']'
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

(<=>) :: Doc -> Doc -> Doc
d1 <=> d2 = d1 <+> equals <+> d2


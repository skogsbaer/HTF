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

Internal module for types and functions dealing with source code locations.

-}

module Test.Framework.Location (

  Location, unknownLocation,

  fileName, lineNumber,

  showLoc, makeLoc

) where


-- | An abstract type representing locations in a file.
data Location = Location String Int
                deriving (Eq, Ord, Show, Read)

-- | Render a 'Location' as a 'String'.
showLoc :: Location -> String
showLoc (Location f n) = f ++ ":" ++ show n

-- | Extract the file name of a 'Location'.
fileName :: Location -> String
fileName (Location f _ ) = f

-- | Extract the line number of a 'Location'.
lineNumber :: Location -> Int
lineNumber (Location _ i) = i

-- | Create a new location.
makeLoc :: String -- ^ The file name
        -> Int    -- ^ The line number
        -> Location
makeLoc = Location

-- | The unknown location (file @?@ and line @0@).
unknownLocation :: Location
unknownLocation = Location "?" 0

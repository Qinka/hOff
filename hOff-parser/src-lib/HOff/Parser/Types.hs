{-
    This file is part of hOff-parser.

    hOff-parser is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    hOff-parser is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
-}

{-|
Module:       HOff.Parser.Types
Description:  Types used in parser.
Copyright:    (C) Johann Lee <me@qinka.pro>, 2017
License:      GPL-3
Maintainer:   me@qinka.pro
Stability:    experimental
Portability:  unknown

The types are used in parser.
-}

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}

module HOff.Parser.Types
  ( -- * Types of OFF
    OFF(..)
  , Vertice(..)
  , Face(..)
  , ParStat(..)
    -- * Functions
  , offVertice
  , offFace
  ) where

import GHC.Exts (IsList(..))

-- | Basic format of file
--   It will include only the vertices and faces.
--   a is type for Vertice
--   b is type for faces' index
--   For example, type SimpleOFF = OFF [] Float.
data OFF a b = OFF [Vertice a] [Face b]
               deriving (Eq,Show)
  
-- | Vertices of OFF in x, y, z coordinate.
--   a is type of value
data Vertice a = Vertice (a,a,a)
               deriving (Eq,Show)

instance Ord a => Ord (Vertice a) where
  compare (Vertice (a,b,c)) (Vertice (x,y,z)) =
    compare (maximum [a,b,c]) (maximum [x,y,z])
  
-- | Face of OFF store the index of every vertices.
--   l is vector should be @* -> *@, and it will be used as array, or say vector.
--   a is type of value
data Face a = Face [a]
            deriving (Eq,Show)

-- | Get vertices from OFF
offVertice :: OFF a b        -- ^ OFF
           -> [Vertice a]  -- ^ Vertices
offVertice (OFF v f) = v

-- | Get faces from OFF
offFace :: OFF a b -- ^ OFF
        -> [Face b] -- Faces
offFace (OFF v f) = f

-- | Status of parser. To get number of vertices and faces.
data ParStat = ParStat
               { numVertices :: Int -- ^ number of vertices
               , numFaces    :: Int -- ^ number of faces
               }
             deriving (Eq,Show)

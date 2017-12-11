{-
    This file is part of hOff-display.

    hOff-display is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    hOff-display is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
-}

{-|
Module:       HOff.Display
Description:  Functions for display
Copyright:    (C) Johann Lee <me@qinka.pro>, 2017
License:      GPL-3
Maintainer:   me@qinka.pro
Stability:    experimental
Portability:  unknown


-}

module HOff.Display
  ( toMesh
  , plotOFF
  ) where

import HOff.Parser
import Graphics.Gnuplot.Simple
import qualified Graphics.Gnuplot.Value.Tuple as Tuple
import qualified Graphics.Gnuplot.Value.Atom as Atom

-- | Translate OFF to mesh
toMesh :: OFF a Int -> [[(a,a,a)]]
toMesh (OFF v f) =
  let ps = map (\(Vertice (x,y,z)) -> (x,y,z)) v
      fs = map (\(Face is) -> is) f
  in map (map (ps !!)) fs

-- | plot for OFF
plotOFF :: (Atom.C a, Tuple.C a)
        => [Attribute]
        -> [Attribute3d]
        -> OFF a Int
        -> IO ()
plotOFF as as3 o = plotMesh3d as as3 (toMesh o)

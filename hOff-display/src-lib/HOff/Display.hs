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
  , off3D
  ) where

import HOff.Parser
import Graphics.Gnuplot.Simple
import qualified Graphics.Gnuplot.Value.Tuple as Tuple
import qualified Graphics.Gnuplot.Value.Atom as Atom
import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Graph as Graph
import qualified Graphics.Gnuplot.Plot.ThreeDimensional as Plot3D
import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.Plot as Plot
import Data.List
import Data.Foldable

-- | Translate OFF to mesh
toMesh :: OFF a Int -> [[(a,a,a)]]
toMesh (OFF v f) =
  let ps = map (\(Vertice (x,y,z)) -> (x,y,z)) v
      fs = map (\(Face is) -> take (length is + 1) $ is ++ is) f
  in map (map (ps !!)) fs

-- | plot for OFF
plotOFF :: (Atom.C a, Tuple.C a)
        => [Attribute]
        -> [Attribute3d]
        -> OFF a Int
        -> IO ()
plotOFF as as3 o = plotMesh3d as as3 (toMesh o)

defltOpts :: Graph.C graph => Opts.T graph
defltOpts =
   Opts.key False $
   Opts.deflt

-- | plot for OFF
off3D :: OFF Float Int
      -> Frame.T (Graph3D.T Float Float Float)
--off3D o = Frame.cons defltOpts $ Plot3D.mesh (toMesh o)
off3D o =
  let mesh = toMesh o
  --in Frame.cons defltOpts $ Plot3D.cloud Graph3D.lines $ concat mesh
  in Frame.cons defltOpts $ foldl' mappend mempty $ 
     map (\m -> Plot3D.cloud Graph3D.lines m) mesh

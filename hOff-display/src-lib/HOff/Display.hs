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
  ( drawOFF
  ) where

import HOff.Parser as HOff
import Graphics.Rendering.OpenGL as GL
import Data.List
import Data.Foldable

-- | draw OFF
drawOFF :: (VertexComponent a, ColorComponent a,Floating a,Show a)
        => OFF a Int
        -> IO ()
drawOFF (OFF ps fs) =  mapM_ (drawFace ps) fs

-- | frace a single face
drawFace :: (VertexComponent a, ColorComponent a,Floating a,Show a)
         => [Vertice a] -- vertices
         -> HOff.Face Int -- a face index
         -> IO ()
drawFace ps (Face is) = GL.renderPrimitive GL.Polygon $ do
  GL.color faceColor
  mapM_ (ver . (ps!!)) is
  where
    vA = diffTriTuple (ps !! (is !! 0)) (ps !! (is !! 1))
    vB = diffTriTuple (ps !! (is !! 1)) (ps !! (is !! 2))
    k  = cosZAngVec $ vA `xProd` vB
    faceColor = toColor $ (k*0.5+0.5) `cProd` (1,0,0)
    ver (Vertice (a,b,c)) = GL.vertex $ GL.Vertex3 a b c

diffTriTuple :: Num a => Vertice a -> Vertice a -> (a,a,a)
diffTriTuple (Vertice (a,b,c)) (Vertice (x,y,z)) = (a-x,b-y,c-z)

xProd :: Num a => (a,a,a) -> (a,a,a) -> (a,a,a)
xProd (a,b,c) (x,y,z) = (b * z - y * c, a * z - x * c, a * y - x * b)

cosZAngVec :: Floating a =>  (a,a,a) -> a
cosZAngVec (a,b,c) = b / sqrt (a^2 + b^2 + c^2)

cProd :: Num a => a -> (a,a,a) -> (a,a,a)
cProd k (a,b,c) = (k*a, k*b, k*c)

toColor :: (a,a,a) -> Color3 a
toColor (a,b,c) = Color3 a b c

moveColor :: Num a => a -> a -> (a,a,a) -> (a,a,a)
moveColor k b (x,y,z) = ( k * x + b
                        , k * y + b
                        , k * z + b
                        )

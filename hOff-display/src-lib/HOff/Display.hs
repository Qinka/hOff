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
  ( transOFF
  , drawMesh
  ) where

import           Data.Foldable
import           Data.List
import           Foreign
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Storable
import           Graphics.Rendering.OpenGL as GL hiding (Face)
import           HOff.Parser               as HOff

arrayWithPtr :: Storable a
             => [a]
             -> (Ptr a -> IO b)
             -> IO b
arrayWithPtr arr func = do
  let len = length arr
  fps <- mallocForeignPtrArray len
  withForeignPtr fps $ \ ptr -> do
    pokeArray ptr arr
    func ptr

oneone :: [Float] -> [Float]
oneone ps =
  let upZero  = map (\i -> i - minimum ps) ps
      oneZero = map (/maximum upZero) upZero
  in map (\i -> i*2-1) oneZero

drawMesh :: [Float] -- points
         -> [Float] -- colors
         -> IO ()
drawMesh ps' cs = do
  let ps = oneone ps'
  clientState VertexArray $= Enabled
  clientState ColorArray  $= Enabled
  arrayWithPtr ps $ \pps ->
    arrayWithPtr cs $ \pcs -> do
      let vP = VertexArrayDescriptor 3 GL.Float 0 pps
          vC = VertexArrayDescriptor 3 GL.Float 0 pcs
      arrayPointer VertexArray $= vP
      arrayPointer ColorArray  $= vC
      drawArrays Triangles 0 (fromIntegral $ length ps `div` 3)
  clientState ColorArray  $= Disabled

mapIdx :: [a] -> [Int] -> [a]
mapIdx xs idx = [xs !! a| a <- idx]

offToTriangle :: Face a -> [[a]]
offToTriangle (Face xs) =
  let m = length xs - 1
      idx = [[a,b,c]| a <- [0..m], b<-[a..m], c <- [b..m], a/=b,b/=c,a/=c]
  in map (mapIdx xs) idx

toPointsColors :: Floating a
               => [Vertice a]
               -> ([a],[a])
toPointsColors (Vertice (a1,a2,a3):Vertice (b1,b2,b3):Vertice (c1,c2,c3):_) =
  let points = [a1,a2,a3,b1,b2,b3,c1,c2,c3]
      k = cosZAngVec $ (a1-b1,a2-b2,a3-b3) `xProd` (b1-c1,b2-c2,b3-c3)
      c = k *0.5+0.5
  in ([a1,a2,a3,b1,b2,b3,c1,c2,c3], [0,0,c,0,0,c,0,0,c])


-- | draw OFF
transOFF :: (VertexComponent a, ColorComponent a,Floating a,Show a)
         => OFF a Int
         -> ([a], [a])
transOFF (OFF ps fs) =
  let idx = concatMap offToTriangle fs
      mappedPoints = map (mapIdx ps) idx
      (pv,cv) = unzip $ map toPointsColors mappedPoints
  in (concat pv, concat cv)

 -- mapM_ (drawFace ps) fs

-- | frace a single face
-- drawFace :: (VertexComponent a, ColorComponent a,Floating a,Show a)
--          => [Vertice a] -- vertices
--          -> HOff.Face Int -- a face index
--          -> IO ()
-- drawFace ps (Face is) = GL.renderPrimitive GL.Polygon $ do
--   GL.color faceColor

--   mapM_ (ver . (ps!!)) is

--   where
--     vA = diffTriTuple (ps !! (is !! 0)) (ps !! (is !! 1))
--     vB = diffTriTuple (ps !! (is !! 1)) (ps !! (is !! 2))
--     k  = cosZAngVec $ vA `xProd` vB
--     faceColor = toColor $ (k*0.5+0.5) `cProd` (1,0,0)
--     ver (Vertice (a,b,c)) = GL.vertex $ GL.Vertex3 a b c

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

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
Module:       Main
Description:  Main file
Copyright:    (C) Johann Lee <me@qinka.pro>, 2017
License:      GPL-3
Maintainer:   me@qinka.pro
Stability:    experimental
Portability:  unknown

-}

module Main where

import           Control.Monad
import           Graphics.Rendering.OpenGL as GL
import           Graphics.Rendering.OpenGL (($=))
import           Graphics.UI.GLFW          as GLFW
import           HOff.Display
import           HOff.Parser
import           System.Environment
import           Text.Parsec


main :: IO ()
main = do
  files <- getArgs
  mapM_ step files
  where step fp' = do
          let fpO = fp ++ ".off"
              fpP = fp ++ ".png"
              fp = real fp'
          str <- readFile fpO
          case runParser offP defParStat fpO str of
            Left  e -> print e
            Right o -> display o
        real str =
          let ext = takeWhile (/='.') $ reverse str
          in if ext == "ffo"
             then reverse $ dropWhile (=='.') $ dropWhile (/='.') $ reverse str
             else str



color3 :: GLfloat -> GLfloat -> GLfloat -> GL.Color3 GLfloat
color3 = GL.Color3

display :: OFF Float Int -> IO ()
display o@(OFF ps fs) = do
  GLFW.initialize
  -- open window
  GLFW.openWindow (GL.Size 400 400) [DisplayAlphaBits 8,DisplayDepthBits 32] GLFW.Window
  GLFW.windowTitle $= "hOff Display"

  GL.shadeModel    $= GL.Smooth
  -- enable antialiasing
  GL.lineSmooth $= GL.Enabled
  GL.blend      $= GL.Enabled
  GL.blendFunc  $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.lineWidth  $= 1.5

  -- depth
  GL.depthClamp  $= Enabled
  GL.depthFunc   $= Just GL.Less
  -- GL.depthRange $= Just (0,1)
  -- GL.blend       $= Enable

  -- set the color to clear background
  GL.clearColor $= Color4 1 1 1 0

  -- OpenGL Viewport.
  GLFW.windowSizeCallback $= \ size@(GL.Size w h) ->
    GL.viewport   $= (GL.Position 0 0, size)
  -- Draw loop
  drawLoop $ transOFF o

  -- finish up
  GLFW.closeWindow
  GLFW.terminate


drawLoop o@(ps,cs) = do
  wO <- getParam Opened
  esc <- GLFW.getKey GLFW.ESC
  when ( esc /= GLFW.Press && wO) $ do
    GLFW.pollEvents
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]

    drawMesh ps cs

    up    <- GLFW.getKey 'W'
    down  <- GLFW.getKey 'S'
    left  <- GLFW.getKey 'A'
    right <- GLFW.getKey 'D'
    zL    <- GLFW.getKey 'Q'
    zR    <- GLFW.getKey 'E'
    zO    <- GLFW.getKey '='
    zI    <- GLFW.getKey '-'

    when (up    == GLFW.Press) $ GL.rotate ( 10 :: Double) (Vector3 1 0 0)
    when (down  == GLFW.Press) $ GL.rotate (-10 :: Double) (Vector3 1 0 0)
    when (left  == GLFW.Press) $ GL.rotate ( 10 :: Double) (Vector3 0 1 0)
    when (right == GLFW.Press) $ GL.rotate (-10 :: Double) (Vector3 0 1 0)
    when (zL    == GLFW.Press) $ GL.rotate ( 10 :: Double) (Vector3 0 0 1)
    when (zR    == GLFW.Press) $ GL.rotate (-10 :: Double) (Vector3 0 0 1)
    when (zO    == GLFW.Press) $ GL.scale 1.1 1.1 (1.1 :: Double)
    when (zI    == GLFW.Press) $ GL.scale 0.9 0.9 (0.9 :: Double)

    GL.flush
    GL.finish

    GLFW.swapBuffers
    GLFW.sleep 0.1

    drawLoop o

v3 a b c = Vertex3 a b c :: Vertex3 Float

normOFF :: (Ord a, Floating a)
        => OFF a b
        -> OFF a b
normOFF (OFF ps fs) =
  let Vertice (a,b,c) = maximum ps
      m = maximum [a,b,c] * 2
      ps' = map (\(Vertice (x,y,z)) -> (Vertice (x/m,y/m,z/m))) ps
  in OFF ps' fs

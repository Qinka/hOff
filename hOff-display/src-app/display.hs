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

import HOff.Parser
import HOff.Display
import Graphics.Gnuplot.Simple
import Control.Monad
import System.Environment
import Text.Parsec
import qualified Graphics.Gnuplot.Advanced as GP
import qualified Graphics.Gnuplot.Terminal.WXT as WXT
import qualified Graphics.Gnuplot.Terminal.PNG as PNG


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
            Right o -> do
              print o
              print $ toMesh o
              GP.plotDefault (off3D (o :: OFF Float Int)) >>= print
        real str =
          let ext = takeWhile (/='.') $ reverse str
          in if ext == "ffo"
             then reverse $ dropWhile (=='.') $ dropWhile (/='.') $ reverse str
             else str

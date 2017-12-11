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
Module:       HOff.Parser
Description:  Parser to parse the OFF(Object File Format, Princeton ModelNet)
Copyright:    (C) Johann Lee <me@qinka.pro>, 2017
License:      GPL-3
Maintainer:   me@qinka.pro
Stability:    experimental
Portability:  unknown

The types are used in parser.
-}

{-# LANGUAGE FlexibleContexts #-}

module HOff.Parser
  ( offP
  , defParStat
  , module HOff.Parser.Types
  )where

import HOff.Parser.Types
import Text.Parsec
import GHC.Exts (IsList(..))
import Data.Char
import Control.Monad


{-|
The example of OFF(Object File Format, Princeton ModelNet).

@
OFF
6 5 0
0 0 0
0 1 0
1 0 0
0 0 1
0 1 1
1 0 1
3 1 0 2
3 4 3 5
4 0 1 4 3
4 0 2 5 3
4 1 2 5 4
@

-}

{-|
TODO the description of OFF's details
-}


-- | Check whether there is ASCII head "OFF"
checkHeadP :: Stream s m Char
           => ParsecT s u m ()
checkHeadP = do
  spaces
  str <- map toLower <$>  many (oneOf "oOfF")
  if str == "off"
    then spaces
    else fail "fail to read OFF head"

-- | Read the length of vertices and faces
readLengthP :: Stream s m Char
            => ParsecT s ParStat m ()
readLengthP = do
  numV:numF:_ <- replicateM 3 readNumP
  spaces
  modifyState (\u -> u { numVertices = numV
                       , numFaces    = numF
                       })

-- | Read the vertices (with contexts)
verticesP :: (Stream s m Char, Read a, Num a)
          => ParsecT s ParStat m [Vertice a]
verticesP = do
  n <- numVertices <$> getState
  replicateM n verticesPStep
  where verticesPStep = do
          x:y:z:_ <- replicateM 3 readNumP
          return $ Vertice (x,y,z)

-- | Read the faces (with contexts)
facesP :: Stream s m Char
       => ParsecT s ParStat m [Face Int]
facesP = do
  n <- numFaces <$> getState
  replicateM n facesPStep
  where facesPStep = do
          n <- readNumP
          Face <$> replicateM n readNumP

-- | Read a number
readNumP :: (Read a, Num a, Stream s m Char)
      => ParsecT s u m a
readNumP = do
  spaces
  read <$> many1 (hexDigit <|> oneOf "+-.")


-- | parser the OFF file
offP :: (Stream s m Char, Read a, Num a)
     => ParsecT s ParStat m (OFF a Int)
offP = do
  checkHeadP
  readLengthP
  vs <- verticesP
  fs <- facesP
  return $ OFF vs fs

-- | default par stat
defParStat :: ParStat
defParStat = ParStat 0 0

{-# LANGUAGE OverloadedStrings #-}
module Gis.Saga.Commands where

import Prelude hiding (FilePath)
import Turtle

-- | Dissolve Polygon with  Tn
polygonsDissolve :: MonadIO io => FilePath -> FilePath -> io ExitCode
polygonsDissolve infile outfile =
  proc
    "saga_cmd"
    ["shapes_polygons", "5", "-POLYGONS", toText' infile, "-DISSOLVED", toText' outfile]
    empty

-- | Dissolve Lines with  Tn
linesDissolve :: MonadIO io => FilePath -> FilePath -> io ExitCode
linesDissolve infile outfile =
  proc
    "saga_cmd"
    ["shapes_lines", "5", "-Lines", toText' infile, "-DISSOLVED", toText' outfile]
    empty

-- | Clip Polygons with Polygons
clipPolygonPolygon :: FilePath -> FilePath -> FilePath -> IO ()
clipPolygonPolygon infile outfile polyClipFile = undefined

-- | Clip Lines with Polygons
clipLinesPolygon :: FilePath -> FilePath -> FilePath -> IO ()
clipLinesPolygon infile outfile polyClipFile = undefined

lineSimplification :: FilePath -> FilePath -> Double -> IO ()
lineSimplification  infile outfile tolerance = undefined

toText' :: FilePath -> Text
toText' = format fp

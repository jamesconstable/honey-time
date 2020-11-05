{-# LANGUAGE OverloadedStrings #-}

module HoneyTime (clockDial, svg) where

import Data.Foldable (fold, foldMap)
import Graphics.Svg
import qualified Data.Text as T

tshow :: Show a => a -> T.Text
tshow = T.pack . show

polar :: (RealFloat a) => (a -> a -> b) -> a -> a -> b
polar fn r theta = fn (r * cos theta) (r * sin theta)

apothem :: (Integral a, RealFloat b) => a -> b -> b
apothem n radius = radius * cos (pi / fromIntegral n)

polygonAngle :: (Integral a, RealFloat b) => a -> Bool -> a -> b
polygonAngle n vertexAtTop i =
  let
    sides  = fromIntegral n
    offset = if vertexAtTop then 0 else pi/sides
  in (fromIntegral i) * 2*pi/sides - pi/2 + offset

hexagonAngle :: (Integral a, RealFloat b) => Bool -> a -> b
hexagonAngle = polygonAngle 6

point :: RealFloat a => a -> a -> T.Text
point x y = T.concat [toText x, " ", toText y, " "]

arc :: RealFloat a => a -> a -> a -> T.Text
arc targetAngle radius rotation = T.intercalate " " [
  "A",                                -- command for arc with absolute values
  toText radius,                      -- x-radius
  toText radius,                      -- y-radius
  toText (abs rotation),              -- x-axis rotation
  "0",                                -- large arc flag
  if rotation > 0 then "1" else "0",  -- sweep direction flag
  polar point radius targetAngle]     -- target coordinate

polygon :: (Integral a, RealFloat b) => a -> b -> Bool -> Element
polygon n r vertexAtTop =
  let
    element p    = polygon_ [Points_ <<- p]
    calcVertex i = polar point r (polygonAngle n vertexAtTop i)
  in element $ foldMap calcVertex [0..(n-1)]

hexagon :: RealFloat a => a -> Bool -> Element
hexagon = polygon 6

annulusSector :: (Integral a, Show a, RealFloat b)
              => a -> b -> b -> a -> Element
annulusSector n r width i =
  let
    theta = 2 * pi / fromIntegral n
    start = polygonAngle n True i
    end   = polygonAngle n True (i+1)
  in path_ [
    Class_ <<- ("sector" <> tshow i),
    D_ <<- fold [
      polar mA r start,               -- move to start position
      arc end r theta,                -- draw outer arc
      polar lA (r-width) end,         -- draw straight line to inner arc
      arc start (r-width) (-theta),   -- draw inner arc
      z]]                             -- draw straight line back to start

hexTranslateHelper :: (RealFloat a, Integral b) => a -> a -> Bool -> b -> T.Text
hexTranslateHelper r m vertexAtTop i =
  let multiplier = if vertexAtTop then r else apothem 6 r
  in polar translate (m * multiplier) (hexagonAngle vertexAtTop i)

hexagonFloret :: RealFloat a => T.Text -> a -> T.Text -> Element
hexagonFloret name tileRadius useId =
  let
    group = g_ [Class_ <<- name]
    usage i = use_ [
      XlinkHref_ <<- useId,
      Class_     <<- "cell" <> tshow i,
      Transform_ <<- hexTranslateHelper tileRadius 2 False i]
  in group $ foldMap usage [0..5]

innerClockDial :: RealFloat a => a -> T.Text -> Element
innerClockDial tileRadius useId =
  let
    group = g_ [Class_ <<- "inner-clock-dial"]
    florets = do
      unit  <- ["subsecond", "second", "minute"]
      place <- ["units", "sixes"]
      return $ T.concat [unit, "-", place]
    createFloret (i, name) = with (hexagonFloret name tileRadius useId) [
      Transform_ <<- hexTranslateHelper tileRadius 8 False i]
  in group $ foldMap createFloret (zip [0..] florets)

outerClockDial :: RealFloat a => a -> Element
outerClockDial tileRadius =
  let
    group = g_ [Class_ <<- "hours-ring"]
    createSector = annulusSector 10 (tileRadius * 11) tileRadius
  in group $ foldMap createSector [0..9]

clockDialDecoration :: RealFloat a => a -> T.Text -> Element
clockDialDecoration tileRadius useId =
  let
    group    = g_ [Class_ <<- "clock-decoration"]
    isEven n = n `mod` 2 == 0
    angleAt  = hexagonAngle True
    withThickStroke = flip with [
      Stroke_       <<- "black",
      Stroke_width_ <<- tshow 6]
    centreCircle = withThickStroke $ circle_ [
        R_    <<- toText (4 * apothem 6 tileRadius),
        Fill_ <<- "white"]
    centreHexagon = withThickStroke $ hexagon (tileRadius * 4) True
    createSpoke i = fold [
      withThickStroke $ path_ [
        D_ <<- fold [
          polar mA (tileRadius * 4) (angleAt i),
          polar lA (tileRadius * (if isEven i then 10 else 8)) (angleAt i),
          z]],
      use_ [
        XlinkHref_ <<- useId,
        Transform_ <<- hexTranslateHelper tileRadius 8 True i],
      withThickStroke $ use_ [
        XlinkHref_ <<- useId,
        Transform_ <<- hexTranslateHelper tileRadius 4 True i]]
  in group (centreHexagon <> foldMap createSpoke [0..5] <> centreCircle)

clockDial :: (RealFloat a) => a -> Element
clockDial tileRadius =
  let tileId = "#hex-tile"
  in g_
    [Class_ <<- "clock-dial"]
    (fold [
      defs_ [] (with (hexagon tileRadius True) [Id_ <<- T.tail tileId]),
      clockDialDecoration tileRadius tileId,
      innerClockDial tileRadius tileId,
      outerClockDial tileRadius])

svg :: Element -> Element
svg content =
  let
    tileRadius  = 10
    widthHeight = tileRadius * 17 * 2
    topLeft     = widthHeight / (-2)
  in
    doctype
    <> with (svg11_ content) [
      Version_ <<- "1.1",
      Width_   <<- "250",
      Height_  <<- "250",
      ViewBox_ <<- point topLeft topLeft <> point widthHeight widthHeight]

{-# LANGUAGE OverloadedStrings #-}

module HoneyTime (clockDial, dateDial, mythDial, svg) where

import Data.Foldable (fold, foldMap)
import Data.Maybe (fromMaybe)
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

annulusSector :: (Integral a, RealFloat b) => a -> b -> b -> Element
annulusSector n outerRadius innerRadius =
  let
    theta = 2 * pi / fromIntegral n
    start = polygonAngle n True 0
    end   = polygonAngle n True 1
  in path_ [
    D_ <<- fold [
      polar mA outerRadius start,     -- move to start position
      arc end outerRadius theta,      -- draw outer arc
      polar lA innerRadius end,       -- draw straight line to inner arc
      arc start innerRadius (-theta), -- draw inner arc
      z]]                             -- draw straight line back to start

createRing :: (Integral a, Show a, RealFloat b)
           => a -> b -> b -> T.Text -> Element
createRing n outerRadius innerRadius className =
  let
    useId = className <> "-template"
    template = defs_ [] $
      with (annulusSector n outerRadius innerRadius) [Id_ <<- useId]
    createUsage i = use_ [
      XlinkHref_ <<- "#" <> useId,
      Class_     <<- "cell cell" <> tshow i,
      Transform_ <<- rotate (fromIntegral i * 360 / fromIntegral n)]
  in g_ [Class_ <<- className] $ (template <> foldMap createUsage [0..(n-1)])

hexagonSector :: RealFloat b => b -> b -> Element
hexagonSector apo innerRadius =
  let
    start = hexagonAngle True 0
    end = hexagonAngle True 1
    circumradius = apo / cos (pi/6)
  in path_ [
    D_ <<- fold [
      polar mA apo start,                    -- move to start position
      polar lA circumradius ((start+end)/2), -- draw line to outermost point
      polar lA apo end,                      -- draw line to far edge
      polar lA innerRadius end,              -- draw line to inner edge
      polar lA innerRadius start,            -- draw inside line
      z]]                                    -- draw line back to start

createHexagonRing :: RealFloat b => b -> b -> T.Text -> Element
createHexagonRing apo innerRadius className =
  let
    useId = className <> "-template"
    template = defs_ [] $ with (hexagonSector apo innerRadius) [Id_ <<- useId]
    createUsage i = use_ [
      XlinkHref_ <<- "#" <> useId,
      Class_     <<- "cell cell" <> tshow i,
      Transform_ <<- rotate (fromIntegral i * 360 / 6)]
  in g_ [Class_ <<- className] $ (template <> foldMap createUsage [0..5])

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
      Class_     <<- "cell cell" <> tshow i,
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
  createRing 10 (tileRadius * 11) (tileRadius * 10) "hours-ring"

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
  let
    tileId = "#hex-tile"
    useId = "#clock-dial"
    mainGroup = g_
      [Id_ <<- T.tail useId, Class_ <<- "clock-dial"]
      (fold [
        defs_ [] (with (hexagon tileRadius True) [Id_ <<- T.tail tileId]),
        clockDialDecoration tileRadius tileId,
        innerClockDial tileRadius tileId,
        outerClockDial tileRadius])
  in mainGroup <> use_ [XlinkHref_ <<- useId, Class_ <<- "solid-color-layer"]

mythDial :: (RealFloat a) => a -> Element
mythDial tileRadius =
  let
    innerDivide  = tileRadius * 2.5
    middleDivide = tileRadius * 8.5
    dialSize     = tileRadius * 11
  in g_ [Class_ <<- "myth-dial"] $ fold [
    createRing 9 middleDivide innerDivide "myth-role",
    createRing 40 dialSize middleDivide "myth-number",
    with (polygon 9 (innerDivide + tileRadius/2) True)
      [Fill_ <<- "white", Stroke_ <<- "black", Stroke_width_ <<- "2"]]

createDotMarkers :: (Integral a, RealFloat b)
                 => a -> (a -> Bool) -> b -> b -> T.Text -> Element
createDotMarkers n skip radius dotRadius className =
  let
    useId = className <> "-template"
    circle = polar (\x y -> circle_ [Cx_ <<- toText x, Cy_ <<- toText y])
    template = defs_ [] $ with (circle radius (-pi/2)) [
      Id_ <<- useId,
      R_ <<- toText dotRadius]
    createUsage (i, x) = use_ [
      XlinkHref_ <<- "#" <> useId,
      Class_     <<- "cell cell" <> tshow i,
      Transform_ <<- rotate (fromIntegral x * 360 / fromIntegral n)]
  in g_ [Class_ <<- className] $
    template <> foldMap createUsage (zip [0..] $ filter skip [0..(n-1)])

dateDial :: (RealFloat a) => a -> Element
dateDial tileRadius =
  let
    divide0  = tileRadius * 1.5
    divide1  = tileRadius * 4.75
    divide2  = tileRadius * 8.5
    divide3  = tileRadius * 9
    dialSize = tileRadius * 11
  in g_ [Class_ <<- "date-dial"] $ fold [
    createRing 30 dialSize divide3 "day-of-month",
    createRing 12 divide3 divide1 "month",
    createDotMarkers 72 (\x -> x `mod` 6 /= 0) divide2 2 "week",
    createHexagonRing divide1 divide0 "season"]

svg :: T.Text -> Element -> Element
svg className content =
  let
    tileRadius  = 12
    widthHeight = tileRadius * 12 * 2
    topLeft     = widthHeight / (-2)
  in
    doctype
    <> with (svg11_ content) [
      Version_ <<- "1.1",
      Class_   <<- className,
      Width_   <<- tshow widthHeight,
      Height_  <<- tshow widthHeight,
      ViewBox_ <<- point topLeft topLeft <> point widthHeight widthHeight]

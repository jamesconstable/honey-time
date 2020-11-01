{-# LANGUAGE OverloadedStrings #-}

module HoneyTime (clockDial, svg) where

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

hexagonAngle :: (Integral a, RealFloat b) => a -> b
hexagonAngle = polygonAngle 6 False

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
  toText (radius * cos targetAngle),  -- target x-coordinate
  toText (radius * sin targetAngle),  -- target y-coordinate
  ""]

polygon :: (Integral a, RealFloat b) => a -> b -> Bool -> Element
polygon n r vertexAtTop =
  let
    element p    = polygon_ [Points_ <<- p]
    calcVertex i = polar point r (polygonAngle n vertexAtTop i)
  in element $ mconcat $ map calcVertex [0..(n-1)]

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
    D_ <<- (
      polar mA r start
      <> arc end r theta
      <> polar lA (r-width) end
      <> arc start (r-width) (-theta)
      <> z)]

hexagonFloret :: RealFloat a => T.Text -> a -> T.Text -> Element
hexagonFloret name radius useId =
  let
    group = g_ [Class_ <<- name]
    calcTranslate i = polar translate (2 * apothem 6 radius) (hexagonAngle i)
    usage i = use_ [
      XlinkHref_ <<- useId,
      Class_     <<- "cell" <> tshow i,
      Transform_ <<- calcTranslate i]
  in group $ mconcat $ map usage [0..5]

innerClockDial :: RealFloat a => a -> T.Text -> Element
innerClockDial radius useId =
  let
    group = g_ [Class_ <<- "inner-clock-dial"]
    florets = do
      unit  <- ["subsecond", "second", "minute"]
      place <- ["units", "sixes"]
      return $ T.concat [unit, "-", place]
    calcTranslate i = polar translate (8 * apothem 6 radius) (hexagonAngle i)
    createFloret (i, name) =
      with (hexagonFloret name radius useId) [Transform_ <<- calcTranslate i]
  in group $ mconcat $ map createFloret (zip [0..] florets)

outerClockDial :: RealFloat a => a -> Element
outerClockDial hexRadius =
  let
    group = g_ [Class_ <<- "hours-ring"]
    createSector = annulusSector 10 (hexRadius * 11) hexRadius
  in group $ mconcat $ map createSector [0..9]

clockDial :: Element
clockDial =
  let
    tileId      = "#hex-tile"
    tileRadius  = 10
    imageWidth  = 250
    imageHeight = 250
  in g_ [
    Class_     <<- "clock-dial",
    Transform_ <<- translate (imageWidth / 2) (imageHeight / 2)]
    (
    defs_ [] (with (hexagon tileRadius True) [Id_ <<- T.tail tileId])
    <> innerClockDial tileRadius tileId
    <> outerClockDial tileRadius)

svg :: Element -> Element
svg content =
  doctype
  <> with (svg11_ content) [
    Version_ <<- "1.1", Width_ <<- "250", Height_ <<- "250"]

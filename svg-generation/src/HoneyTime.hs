{-# LANGUAGE OverloadedStrings #-}

module HoneyTime (clockDial, svg) where

import Graphics.Svg
import qualified Data.Text as T

tshow :: Show a => a -> T.Text
tshow = T.pack . show

range :: (Integral a, Num b) => a -> [b]
range n = map fromIntegral [0..(n-1)]

point :: RealFloat a => a -> a -> T.Text
point x y = T.concat [toText x, ",", toText y, " "]

polarTranslate :: RealFloat a => a -> a -> T.Text
polarTranslate r theta = translate (r * cos theta) (r * sin theta)

polygonAngle :: (Integral a, RealFloat b) => a -> a -> b
polygonAngle n i = (fromIntegral i - 1) * 2 * pi / (fromIntegral n)

hexagonAngle :: (Integral a, RealFloat b) => a -> b
hexagonAngle = polygonAngle 6

apothem :: (Integral a, RealFloat b) => a -> b -> b
apothem n radius = radius * cos (pi / fromIntegral n)

polygon :: (Integral a, RealFloat b) => a -> b -> b -> b -> b -> Element
polygon n x y radius rotation =
  let
    calcVertex i =
      let theta = pi/2 - 2*pi*i/(fromIntegral n) - rotation
      in point (x + radius * cos theta) (y - radius * sin theta)
    points = map calcVertex (range n)
  in polygon_ [Points_ <<- mconcat points]

hexagon :: RealFloat a => a -> a -> a -> a -> Element
hexagon = polygon 6

hexagonFloret :: RealFloat a => T.Text -> a -> T.Text -> Element
hexagonFloret name radius useId =
  let
    group = g_ [Class_ <<- name]
    calcTranslate i = polarTranslate (2 * apothem 6 radius) (hexagonAngle i)
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
    calcTranslate i = polarTranslate (8 * apothem 6 radius) (hexagonAngle i)
    createFloret (i, name) =
      with (hexagonFloret name radius useId) [Transform_ <<- calcTranslate i]
  in group $ mconcat $ map createFloret (zip [0..] florets)

arc :: RealFloat a => a -> a -> a -> T.Text
arc targetAngle radius rotation = T.intercalate " " [
  "A",                                -- absolute-valued arc command
  toText radius,                      -- x-radius
  toText radius,                      -- y-radius
  toText (abs rotation),              -- x-axis rotation
  "0",                                -- large arc flag
  if rotation > 0 then "1" else "0",  -- sweep direction flag
  toText (radius * cos targetAngle),  -- target x-coordinate
  toText (radius * sin targetAngle),  -- target y-coordinate
  ""]

hoursRing :: RealFloat a => a -> Element
hoursRing hexRadius =
  let
    group = g_ [Class_ <<- "hours-ring"]
    innerRadius = hexRadius * 10
    outerRadius = hexRadius * 11
    theta = -pi/2
    phi = pi/5
    createSegment i =
      let
        start = theta + phi * (fromIntegral i)
        end   = theta + phi * (fromIntegral i + 1)
      in path_ [
        Class_ <<- ("cell" <> tshow i),
        D_ <<- (
          mA (outerRadius * cos start) (outerRadius * sin start)
          <> arc end outerRadius phi
          <> lA (innerRadius * cos end) (innerRadius * sin end)
          <> arc start innerRadius (-phi)
          <> z)]
  in group $ mconcat $ map createSegment [0..9]

svg :: Element -> Element
svg content =
  doctype
  <> with (svg11_ content) [
    Version_ <<- "1.1", Width_ <<- "500", Height_ <<- "500"]

clockDial :: Element
clockDial =
  let
    tileId = "#hex_tile"
    tileRadius = 15
    centreTranslate = translate (12 * tileRadius) (12 * tileRadius)
  in
    defs_ [] (with (hexagon 0 0 15 0) [Id_ <<- T.tail tileId])
    <> with (innerClockDial tileRadius tileId) [Transform_ <<- centreTranslate]
    <> with (hoursRing tileRadius) [Transform_ <<- centreTranslate]

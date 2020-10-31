{-# LANGUAGE OverloadedStrings #-}

module HoneyTime (clockDial, svg) where

import Graphics.Svg
import qualified Data.Text as T

tshow :: Show a => a -> T.Text
tshow = T.pack . show

point :: RealFloat a => a -> a -> T.Text
point x y = T.concat [toText x, ",", toText y, " "]

range :: (Integral a, Num b) => a -> [b]
range n = map fromIntegral [0..(n-1)]

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

polarTranslate :: RealFloat a => a -> a -> T.Text
polarTranslate r theta = translate (r * cos theta) (r * sin theta)

polygonAngle :: (Integral a, RealFloat b) => a -> a -> b
polygonAngle n i = (fromIntegral i - 1) * 2 * pi / (fromIntegral n)

hexagonAngle :: (Integral a, RealFloat b) => a -> b
hexagonAngle = polygonAngle 6

hexagonFloret :: RealFloat a => T.Text -> a -> T.Text -> Element
hexagonFloret name radius useId =
  let
    group = g_ [Class_ <<- name]
    shortRadius = sqrt (3/4 * radius * radius)
    calcTranslate i = polarTranslate (2 * shortRadius) (hexagonAngle i)
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
    shortRadius = sqrt (3/4 * radius * radius)
    calcTranslate i = polarTranslate (8 * shortRadius) (hexagonAngle i)
    createFloret (i, name) =
      with (hexagonFloret name radius useId) [Transform_ <<- calcTranslate i]
  in group $ mconcat $ map createFloret (zip [0..] florets)

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

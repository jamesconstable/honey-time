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

hexagonFloret :: RealFloat a => T.Text -> a -> T.Text -> Element
hexagonFloret name radius useId =
  let
    shortRadius = sqrt(3/4 * radius * radius)
    calcTranslate i =
      let theta = (fromIntegral i - 1) * pi / 3
      in translate (2 * shortRadius * cos theta) (2 * shortRadius * sin theta)
    usage i = use_ [
      XlinkHref_ <<- useId,
      Class_     <<- "cell" <> tshow i,
      Transform_ <<- calcTranslate i]
  in g_ [Id_ <<- name] (mconcat $ map usage [0..5])

svg :: Element -> Element
svg content =
  doctype
  <> with (svg11_ content) [
    Version_ <<- "1.1", Width_ <<- "100", Height_ <<- "100"]

clockDial :: Element
clockDial =
  let
    tileId = "#hex_tile"
    tileRadius = 15
  in
    defs_ [] (with (hexagon 0 0 15 0) [Id_ <<- T.tail tileId])
    <> with (hexagonFloret "test-ring" tileRadius tileId) [
      Transform_ <<- translate 50 50]

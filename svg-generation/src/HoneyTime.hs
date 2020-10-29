{-# LANGUAGE OverloadedStrings #-}

module HoneyTime (clockDial, svg) where

import Graphics.Svg
import qualified Data.Text as T

p :: RealFloat a => a -> a -> T.Text
p x y = T.concat [toText x, ",", toText y, " "]

svg :: Element -> Element
svg content =
  doctype
  <> with (svg11_ content) [
    Version_ <<- "1.1", Width_ <<- "100", Height_ <<- "100"]

range :: (Integral a, Num b) => a -> [b]
range n = map fromIntegral [0..(n-1)]

polygon :: (Integral a, RealFloat b) => a -> b -> b -> b -> b -> Element
polygon n x y radius rotation =
  let
    theta = pi / 2 - rotation
    polyAngle = 2 * pi / fromIntegral n
    calcPoint i = p
      (x + radius * cos (theta - i*polyAngle))
      (y - radius * sin (theta - i*polyAngle))
    points = map calcPoint (range n)
  in polygon_ [Points_ <<- T.concat points]

hexagon :: RealFloat a => a -> a -> a -> a -> Element
hexagon = polygon 6

clockDial :: Element
clockDial =
  path_ [
    Fill_ <<- "#352950",
    D_ <<- (mA 0 340 <> lA 113 170 <> lA 0 0 <> lA 85 0 <> lA 198 170
      <> lA 85 340 <> lA 0 340 <> z <> mA 0 340 )]
  <> path_ [
    Fill_ <<- "#4A3A74",
    D_ <<- (mA 113 340 <> lA 226 170 <> lA 113 0 <> lA 198 0 <> lA 425 340
      <> lA 340 340 <> lA 269 234 <> lA 198 340 <> lA 113 340 <> z
      <> mA 113 340)]
  <> path_ [
    Fill_ <<- "#7C3679",
    D_ <<- ( mA 387 241 <> lA 350 184 <> lA 482 184 <> lA 482 241 <> lA 387 241
      <> z <> mA 387 241)]
  <> path_ [
    Fill_ <<- "#7C3679",
    D_ <<- ( mA 331 156 <> lA 293 99 <> lA 482 99 <> lA 482 156 <> lA 331 156
      <> z <> mA 331 156)]
  <> polygon 9 50 50 25 0

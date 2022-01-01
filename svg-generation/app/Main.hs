{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO (writeFile)

import HoneyTime (clockDial, dateDial, mythDial, hexBackground,
  hexBackgroundMesh, svg)

main :: IO ()
main = do
  writeFile "output/clock-dial.svg" $ show $ svg "clock-dial" $ clockDial 12
  writeFile "output/date-dial.svg" $ show $ svg "date-dial" $ dateDial 12
  writeFile "output/myth-dial.svg" $ show $ svg "myth-dial" $ mythDial 12
  writeFile "output/background.svg" $ show $ hexBackground 10
  writeFile "output/background-mesh.svg" $ show $ hexBackgroundMesh 10

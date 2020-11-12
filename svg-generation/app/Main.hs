module Main where

import System.IO (writeFile)

import HoneyTime (clockDial, dateDial, mythDial, svg)

main :: IO ()
main = do
  writeFile "output/clock-dial.svg" $ show $ svg $ clockDial 12
  writeFile "output/date-dial.svg" $ show $ svg $ dateDial 12
  writeFile "output/myth-dial.svg" $ show $ svg $ mythDial 12

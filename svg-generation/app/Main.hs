module Main where

import HoneyTime (clockDial, dateDial, mythDial, svg)

main :: IO ()
main = do
  print $ svg $ dateDial 12
  print $ svg $ clockDial 12
  print $ svg $ mythDial 12

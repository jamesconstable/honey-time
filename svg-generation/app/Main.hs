module Main where

import HoneyTime (clockDial, mythDial, svg)

main :: IO ()
main = do
  print $ svg $ clockDial 12
  print $ svg $ mythDial 12

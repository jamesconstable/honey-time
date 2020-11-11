module Main where

import HoneyTime (clockDial, mythDial, svg)

main :: IO ()
main = print $ svg $ mythDial 12

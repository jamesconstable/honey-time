module Main where

import HoneyTime (clockDial, svg)

main :: IO ()
main = print $ svg $ clockDial 15

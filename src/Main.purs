module Main where

import Prelude

import Data.JSDate (JSDate, jsdate)
import Effect (Effect)
import Effect.Console (log)

data MythName = MythName String String
data LetterName = LetterName String String String

creation :: JSDate
creation = jsdate {
    year: 2015.0,
    month: 10.0,
    day: 29.0,
    hour: 23.0,
    minute: 34.0,
    second: 14.0,
    millisecond: 0.0
}

wingflap_millis :: Int
wingflap_millis = 4

subsecond_millis :: Int
subsecond_millis = wingflap_millis * 36

second_millis :: Int
second_millis = subsecond_millis * 36

minute_millis :: Int
minute_millis = second_millis * 36

hour_millis :: Int
hour_millis = minute_millis * 36

day_millis :: Int
day_millis = hour_millis * 10

week_millis :: Int
week_millis = day_millis * 6

month_millis :: Int
month_millis = day_millis * 30

year_millis :: Int
year_millis = day_millis * 360

mythCycle :: Array MythName
mythCycle = [
  MythName "Divolm"    "Thunder",
  MythName "Telzlnoln" "Rain",
  MythName "Jidolk"    "Flower",
  MythName "Shelsheln" "River",
  MythName "Thefam"    "Stone",
  MythName "Zatheln"   "Spider",
  MythName "Kizhult"   "Bee",
  MythName "Thefnolm"  "Bear",
  MythName "Vithit"    "Bird"
]

letterCycle :: Array LetterName
letterCycle = [
  LetterName "Duhdem"     "Dam"       "d",
  LetterName "Gigim"      "Flipper"   "g",
  LetterName "Xataxym"    "Pit"       "x",
  LetterName "Jegen"      "Hook"      "j",
  LetterName "Fijyc"      "Rainbow"   "f",
  LetterName "Voljam"     "Ear"       "v",
  LetterName "Thethat"    "Wind"      "th",
  LetterName "Sekelt"     "Valley"    "s",
  LetterName "Zuhzuhmelt" "Ladle"     "z",
  LetterName "Shuhzhik"   "Tear"      "sh",
  LetterName "Zhizlik"    "Fish"      "zh",
  LetterName "Slik"       "Thumbs-up" "sl",
  LetterName "Zlolfit"    "Wing"      "zl",
  LetterName "Molmelc"    "Roof"      "m",
  LetterName "Nyzlan"     "Snail"     "n",
  LetterName "Nasham"     "Wave"      "a",
  LetterName "Xelteln"    "Cliff"     "el",
  LetterName "Tezet"      "Lightning" "e",
  LetterName "Tolmolm"    "Slope"     "ol",
  LetterName "Mizizlat"   "Cart"      "i",
  LetterName "Slysyc"     "Snake"     "y",
  LetterName "Shnuhk"     "Lips"      "uh",
  LetterName "Tuln"       "Eye"       "ul",
  LetterName "Cuhc"       "Foot"      "c",
  LetterName "Tytyt"      "Clover"    "t",
  LetterName "Kyfik"      "Arm"       "k",
  LetterName "Zlnanic"    "Chameleon" "ah",
  LetterName "Thnuhduhk"  "Elephant"  "eh",
  LetterName "Snolzem"    "Knot"      "o",
  LetterName "Vmyn"       "Mouth"     "u"
]

seasons :: Array String
season = ["Egg", "Larva", "Pupa", "Worker", "Drone", "Queen"]

main :: Effect Unit
main = do
  log "🍝"

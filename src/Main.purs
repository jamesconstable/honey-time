module Main where

import Prelude

import Data.Array (replicate)
import Data.Int (radix, toNumber, toStringAs)
import Data.Int53
import Data.Foldable (fold)
import Data.JSDate (getTime, JSDate, jsdate)
import Data.Maybe (fromJust)
import Data.String (length)
import Data.String.CodeUnits (singleton)
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)

data MythName = MythName String String
data LetterName = LetterName String String String

type HoneyDate = {
    year :: Int,
    month :: Int,
    dayOfYear :: Int,
    dayOfMonth :: Int,
    hours :: Int,
    minutes :: Int,
    seconds :: Int,
    subseconds :: Int,
    mythRole :: Int,
    mythNumber :: Int,
    season :: Int
}

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

wingflapMillis :: Int53
wingflapMillis = fromInt 4

subsecondMillis :: Int53
subsecondMillis = wingflapMillis * fromInt 36

secondMillis :: Int53
secondMillis = subsecondMillis * fromInt 36

minuteMillis :: Int53
minuteMillis = secondMillis * fromInt 36

hourMillis :: Int53
hourMillis = minuteMillis * fromInt 36

dayMillis :: Int53
dayMillis = hourMillis * fromInt 10

weekMillis :: Int53
weekMillis = dayMillis * fromInt 6

monthMillis :: Int53
monthMillis = dayMillis * fromInt 30

yearMillis :: Int53
yearMillis = dayMillis * fromInt 360

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
seasons = ["Egg", "Larva", "Pupa", "Worker", "Drone", "Queen"]

padLeft :: Int -> Char -> String -> String
padLeft width c s
    | length s >= width = s
    | otherwise = fold (replicate (width - length s) (singleton c)) <> s

toSenary :: Int -> Int -> String
toSenary value width =
  let base6 = unsafePartial $ fromJust $ radix 6
  in padLeft width '0' (toStringAs base6 value)

gregorianToHoney :: JSDate -> HoneyDate
gregorianToHoney date =
  let
    millis = floor (getTime date - getTime creation)
    subunits major minor = toInt (millis `mod` major / minor)
    month = subunits yearMillis monthMillis
    dayOfYear = subunits yearMillis dayMillis
  in {
    year:       toInt (millis / yearMillis),
    month:      month,
    dayOfYear:  dayOfYear,
    dayOfMonth: subunits monthMillis dayMillis,
    hours:      subunits dayMillis hourMillis,
    minutes:    subunits hourMillis minuteMillis,
    seconds:    subunits minuteMillis secondMillis,
    subseconds: subunits secondMillis subsecondMillis,
    mythRole:   dayOfYear `mod` 9,
    mythNumber: dayOfYear `mod` 40,
    season:     month / 2
  }

main :: Effect Unit
main = do
  log "🍝"

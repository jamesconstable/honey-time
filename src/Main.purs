module Main where

import Prelude

import Data.Array (unsafeIndex)
import Data.Int (radix, toStringAs)
import Data.Int53 (Int53, floor, fromInt, toInt)
import Data.JSDate (JSDate, getTime, jsdate, now)
import Data.Maybe (fromJust)
import Data.String (joinWith, length)
import Data.String.CodeUnits (singleton)
import Effect (Effect)
import Effect.Console (log)
import Effect.Timer (setInterval)
import Partial.Unsafe (unsafePartial)
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.Element (toNode)
import Web.DOM.Internal.Types (Element)
import Web.DOM.Node (setTextContent)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)

type MythName = {
    sajemTan :: String,
    english :: String
}

mythName :: String -> String -> MythName
mythName sajemTan english = { sajemTan, english }

type LetterName = {
    sajemTan :: String,
    english :: String,
    orthographic :: String
}

letterName :: String -> String -> String -> LetterName
letterName sajemTan english orthographic = { sajemTan, english, orthographic }

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

type TextualDisplay = {
    hours :: Element,
    minutes :: Element,
    seconds :: Element,
    subseconds :: Element,
    date :: Element,
    season :: Element
}

dangerIndex :: forall a. Array a -> Int -> a
dangerIndex = unsafePartial unsafeIndex
infixl 8 dangerIndex as !!!

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

seasonMillis :: Int53
seasonMillis = monthMillis * fromInt 2

yearMillis :: Int53
yearMillis = dayMillis * fromInt 360

mythCycle :: Array MythName
mythCycle = [
  mythName "Divolm"    "Thunder",
  mythName "Telzlnoln" "Rain",
  mythName "Jidolk"    "Flower",
  mythName "Shelsheln" "River",
  mythName "Thefam"    "Stone",
  mythName "Zatheln"   "Spider",
  mythName "Kizhult"   "Bee",
  mythName "Thefnolm"  "Bear",
  mythName "Vithit"    "Bird"
]

letterCycle :: Array LetterName
letterCycle = [
  letterName "Duhdem"     "Dam"       "d",
  letterName "Gigim"      "Flipper"   "g",
  letterName "Xataxym"    "Pit"       "x",
  letterName "Jegen"      "Hook"      "j",
  letterName "Fijyc"      "Rainbow"   "f",
  letterName "Voljam"     "Ear"       "v",
  letterName "Thethat"    "Wind"      "th",
  letterName "Sekelt"     "Valley"    "s",
  letterName "Zuhzuhmelt" "Ladle"     "z",
  letterName "Shuhzhik"   "Tear"      "sh",
  letterName "Zhizlik"    "Fish"      "zh",
  letterName "Slik"       "Thumbs-up" "sl",
  letterName "Zlolfit"    "Wing"      "zl",
  letterName "Molmelc"    "Roof"      "m",
  letterName "Nyzlan"     "Snail"     "n",
  letterName "Nasham"     "Wave"      "a",
  letterName "Xelteln"    "Cliff"     "el",
  letterName "Tezet"      "Lightning" "e",
  letterName "Tolmolm"    "Slope"     "ol",
  letterName "Mizizlat"   "Cart"      "i",
  letterName "Slysyc"     "Snake"     "y",
  letterName "Shnuhk"     "Lips"      "uh",
  letterName "Tuln"       "Eye"       "ul",
  letterName "Cuhc"       "Foot"      "c",
  letterName "Tytyt"      "Clover"    "t",
  letterName "Kyfik"      "Arm"       "k",
  letterName "Zlnanic"    "Chameleon" "ah",
  letterName "Thnuhduhk"  "Elephant"  "eh",
  letterName "Snolzem"    "Knot"      "o",
  letterName "Vmyn"       "Mouth"     "u"
]

seasons :: Array String
seasons = ["Egg", "Larva", "Pupa", "Worker", "Drone", "Queen"]

padLeft :: Int -> Char -> String -> String
padLeft width c s
    | length s >= width = s
    | otherwise         = padLeft width c (singleton c <> s)

toSenary :: Int -> Int -> String
toSenary value width =
  let base6 = unsafePartial $ fromJust $ radix 6
  in padLeft width '0' (toStringAs base6 value)

gregorianToHoney :: JSDate -> HoneyDate
gregorianToHoney date =
  let
    millis = floor (getTime date - getTime creation)
    asSubunitOf minor major = toInt (millis `mod` major / minor)
    dayOfYear = dayMillis `asSubunitOf` yearMillis
  in {
    year:       toInt (millis / yearMillis),
    season:     seasonMillis    `asSubunitOf` yearMillis,
    month:      monthMillis     `asSubunitOf` yearMillis,
    dayOfMonth: dayMillis       `asSubunitOf` monthMillis,
    hours:      hourMillis      `asSubunitOf` dayMillis,
    minutes:    minuteMillis    `asSubunitOf` hourMillis,
    seconds:    secondMillis    `asSubunitOf` minuteMillis,
    subseconds: subsecondMillis `asSubunitOf` secondMillis,
    dayOfYear:  dayOfYear,
    mythRole:   dayOfYear `mod` 9,
    mythNumber: dayOfYear `mod` 40
  }

elementById :: String -> Effect Element
elementById id = do
  w <- window
  d <- document w
  e <- getElementById id $ toNonElementParentNode $ toDocument d
  pure $ unsafePartial $ fromJust e

getTextualDisplay :: Effect TextualDisplay
getTextualDisplay = do
  let get n = elementById ("textual-" <> n)
  hours      <- get "hours"
  minutes    <- get "minutes"
  seconds    <- get "seconds"
  subseconds <- get "subseconds"
  date       <- get "date"
  season     <- get "season"
  pure { hours, minutes, seconds, subseconds, date, season }

setTextualDisplay :: HoneyDate -> TextualDisplay -> Effect Unit
setTextualDisplay date display = do
  let set e s = setTextContent s (toNode e)
  let dateText = joinWith " " [
      show date.year,
      (mythCycle !!! date.mythRole).sajemTan,
      show date.mythNumber <> ",",
      show date.month,
      (letterCycle !!! date.dayOfMonth).sajemTan]
  set display.hours      (show date.hours)
  set display.minutes    (toSenary date.minutes 2)
  set display.seconds    (toSenary date.seconds 2)
  set display.subseconds (toSenary date.subseconds 2)
  set display.date       dateText
  set display.season     (seasons !!! date.season <> " Season")

displayDate :: JSDate -> TextualDisplay -> Effect Unit
displayDate date textDisplay = do
  let honeyDate = gregorianToHoney date
  setTextualDisplay honeyDate textDisplay

displayNow :: TextualDisplay -> Effect Unit
displayNow display = do
  n <- now
  displayDate n display

setup :: Effect Unit
setup = void do
  t <- getTextualDisplay
  setInterval (toInt subsecondMillis) (displayNow t)

main :: Effect Unit
main = mempty

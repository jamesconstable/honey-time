module Main where

import Prelude

import Data.Array ((..), (!!))
import Data.ArrayView (fromArray, take)
import Data.Filterable (filterMap)
import Data.Foldable (fold, foldMap)
import Data.Int (floor, radix, toStringAs)
import Data.JSDate (JSDate, getTime, jsdate, now)
import Data.Maybe (fromJust)
import Data.Monoid (power)
import Data.String (length)
import Data.String.CodeUnits (singleton)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Timer (setInterval)
import Math ((%))
import Partial.Unsafe (unsafePartial)
import Web.DOM.Document (toParentNode)
import Web.DOM.DOMTokenList as TL
import Web.DOM.Element (classList, fromNode, toNode)
import Web.DOM.NodeList (toArray)
import Web.DOM.Internal.Types (Element)
import Web.DOM.Node (setTextContent)
import Web.DOM.ParentNode (QuerySelector(..), querySelectorAll)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)

type TranslatedName = ( sajemTan :: String, english :: String )

type MythName = { | TranslatedName }

type LetterName = { orthographic :: String | TranslatedName }

type HoneyBase a = (
  year :: a,
  season :: a,
  month :: a,
  week :: a,
  hour :: a,
  minute :: a,
  second :: a,
  subsecond :: a
  )

type HoneyComponents a = {
  dayOfYear :: a,
  dayOfMonth :: a,
  mythRole :: a,
  mythNumber :: a
  | HoneyBase a
  }

type HoneyDate = HoneyComponents Int

data DisplayComponent =
  None
  | TextComponent (Array Element) (Int -> String)
  | LinearComponent (Array (Array Element))
  | SenaryComponent (Array (Array Element)) (Array (Array Element))

type Display = HoneyComponents DisplayComponent

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

honeyDurations :: { day :: Number | HoneyBase Number }
honeyDurations =
  let
    wingflap  = 4.0
    subsecond = wingflap * 36.0
    second    = subsecond * 36.0
    minute    = second * 36.0
    hour      = minute * 36.0
    day       = hour * 10.0
    week      = day * 6.0
    month     = day * 30.0
    season    = month * 2.0
    year      = day * 360.0
  in { year, season, month, week, day, hour, minute, second, subsecond }

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
  ] where mythName = { sajemTan: _, english: _ }

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
  ] where letterName = { sajemTan: _, english: _, orthographic: _ }

seasons :: Array String
seasons = ["Egg", "Larva", "Pupa", "Worker", "Drone", "Queen"]

padLeft :: Int -> Char -> String -> String
padLeft width c s = power (singleton c) (width - length s) <> s

toSenary :: Int -> Int -> String
toSenary width value =
  let base6 = unsafePartial $ fromJust $ radix 6
  in padLeft width '0' (toStringAs base6 value)

elementsBySelector :: String -> Effect (Array Element)
elementsBySelector selector = do
  w <- window
  d <- document w
  r <- querySelectorAll (QuerySelector selector) (toParentNode $ toDocument d)
  filterMap fromNode <$> toArray r

addClass :: String -> Element -> Effect Unit
addClass c e = classList e >>= flip TL.add c

removeClass :: String -> Element -> Effect Unit
removeClass c e = classList e >>= flip TL.remove c

gregorianToHoney :: JSDate -> HoneyDate
gregorianToHoney date =
  let
    millis = getTime date - getTime creation
    _of minor major =
      floor (millis % (major honeyDurations) / (minor honeyDurations))
    year       = floor (millis / honeyDurations.year)
    season     = _.season    `_of` _.year
    month      = _.month     `_of` _.year
    week       = _.week      `_of` _.year
    dayOfYear  = _.day       `_of` _.year
    dayOfMonth = _.day       `_of` _.month
    hour       = _.hour      `_of` _.day
    minute     = _.minute    `_of` _.hour
    second     = _.second    `_of` _.minute
    subsecond  = _.subsecond `_of` _.second
    mythRole   = dayOfYear   `mod` 9
    mythNumber = dayOfYear   `mod` 40
  in { year, season, month, dayOfYear, dayOfMonth, week, mythRole, mythNumber,
       hour, minute, second, subsecond }

getTextualDisplay :: Effect Display
getTextualDisplay =
  let
    getClass = (<>) ".textual-display ."
    textComponent n formatFn =
      TextComponent <$> elementsBySelector (getClass n) <@> formatFn
    formatSeason i = fold (seasons !! i) <> " Season"
    formatDay i = (fold (letterCycle !! i)).sajemTan
    formatMythRole i = (fold (mythCycle !! i)).sajemTan
  in ado
    year       <- textComponent "year"        show
    season     <- textComponent "season"      formatSeason
    month      <- textComponent "month"       show
    dayOfMonth <- textComponent "day"         formatDay
    mythRole   <- textComponent "myth-role"   formatMythRole
    mythNumber <- textComponent "myth-number" show
    hour       <- textComponent "hours"       show
    minute     <- textComponent "minutes"     (toSenary 2)
    second     <- textComponent "seconds"     (toSenary 2)
    subsecond  <- textComponent "subseconds"  (toSenary 2)
    in { year, season, month, dayOfMonth, mythRole, mythNumber,
         hour, minute, second, subsecond, week: None, dayOfYear: None }

getGraphicalDisplay :: Effect Display
getGraphicalDisplay =
  let
    linearComponent name n =
      let selectorFor i = fold [".", name, " .cell", show i]
      in LinearComponent <$> traverse elementsBySelector (selectorFor <$> 0..n)
    senaryComponent name =
      let selectorFor p i = fold [".", name, "-", p, " .cell", show i]
      in SenaryComponent
        <$> traverse elementsBySelector (selectorFor "units" <$> 0..5)
        <*> traverse elementsBySelector (selectorFor "sixes" <$> 0..5)
  in ado
    season     <- linearComponent "season"       6
    month      <- linearComponent "month"        12
    week       <- linearComponent "week"         60
    dayOfMonth <- linearComponent "day-of-month" 30
    mythRole   <- linearComponent "myth-role"    9
    mythNumber <- linearComponent "myth-number"  40
    hour       <- linearComponent "hours-ring"   10
    minute     <- senaryComponent "minute"
    second     <- senaryComponent "second"
    subsecond  <- senaryComponent "subsecond"
    in { season, month, week, dayOfMonth, mythRole, mythNumber,
         hour, minute, second, subsecond, year: None, dayOfYear: None }

setDisplay :: HoneyDate -> Display -> Effect Unit
setDisplay date display =
  set _.year *> set _.season *> set _.month *> set _.week *> set _.dayOfYear *>
  set _.dayOfMonth *> set _.mythRole *> set _.mythNumber *>
  set _.hour *> set _.minute *> set _.second *> set _.subsecond
  where
    set :: (forall a. HoneyComponents a -> a) -> Effect Unit
    set getFrom = setComponent (getFrom display) (getFrom date)

    setComponent :: DisplayComponent -> Int -> Effect Unit
    setComponent c n = case c of
      None                        -> mempty
      TextComponent es formatFn   -> setText es (formatFn n)
      LinearComponent es          -> setElements es n
      SenaryComponent units sixes -> setElements units (mod n 6) *>
                                     setElements sixes (n/6)

    setText :: Array Element -> String -> Effect Unit
    setText es t = foldMap (setTextContent t <<< toNode) es

    setElements :: Array (Array Element) -> Int -> Effect Unit
    setElements es digit = do
      foldMap (foldMap $ removeClass "active") es
      foldMap (foldMap $ removeClass "filled") es
      foldMap (addClass "active") (fold $ es !! digit)
      foldMap (foldMap $ addClass "filled") (take digit $ fromArray es)

displayDate :: JSDate -> Display -> Effect Unit
displayDate = setDisplay <<< gregorianToHoney

displayNow :: Array Display -> Effect Unit
displayNow ds = do
  n <- now
  foldMap (displayDate n) ds

setup :: Effect Unit
setup = void do
  t <- getTextualDisplay
  g <- getGraphicalDisplay
  setInterval (floor honeyDurations.subsecond) (displayNow [t, g])

main :: Effect Unit
main = mempty

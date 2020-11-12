module Main where

import Prelude

import Data.Array ((..), (!!))
import Data.ArrayView (fromArray, take)
import Data.Filterable (filterMap)
import Data.Foldable (fold, foldMap)
import Data.Int (floor, radix, toStringAs)
import Data.JSDate (JSDate, getTime, jsdate, now)
import Data.Maybe (Maybe(..), fromJust)
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

type TextualDisplay = HoneyComponents (Maybe Element)

data DisplayComponent =
  None
  | LinearComponent (Array (Array Element))
  | SenaryComponent (Array (Array Element)) (Array (Array Element))

type GraphicalDisplay = HoneyComponents DisplayComponent

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
padLeft width c s
  | length s >= width = s
  | otherwise         = padLeft width c (singleton c <> s)

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

elementById :: String -> Effect (Maybe Element)
elementById = map (_ !! 0) <<< elementsBySelector <<< ("#" <> _)

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

getTextualDisplay :: Effect TextualDisplay
getTextualDisplay =
  let
    get n = elementById ("textual-" <> n)
  in ado
    year       <- get "year"
    season     <- get "season"
    month      <- get "month"
    dayOfMonth <- get "day"
    mythRole   <- get "myth-role"
    mythNumber <- get "myth-number"
    hour       <- get "hours"
    minute     <- get "minutes"
    second     <- get "seconds"
    subsecond  <- get "subseconds"
    in { year, season, month, dayOfMonth, mythRole, mythNumber,
         hour, minute, second, subsecond,
         week: Nothing, dayOfYear: Nothing }

setTextualDisplay :: HoneyDate -> TextualDisplay -> Effect Unit
setTextualDisplay date display =
  let
    setElementText e s = setTextContent s (toNode $ unsafePartial fromJust e)
    with :: (forall a. HoneyComponents a -> a) -> (Int -> String) -> Effect Unit
    with getFrom fn = setElementText (getFrom display) (fn (getFrom date))
  in ado
    _.year       `with` show
    _.season     `with` \s -> (fold (seasons !! s)) <> " Season"
    _.month      `with` show
    _.dayOfMonth `with` \d -> (fold (letterCycle !! d)).sajemTan
    _.mythRole   `with` \m -> (fold (mythCycle !! m)).sajemTan
    _.mythNumber `with` show
    _.hour       `with` show
    _.minute     `with` toSenary 2
    _.second     `with` toSenary 2
    _.subsecond  `with` toSenary 2
    in unit

getGraphicalDisplay :: Effect GraphicalDisplay
getGraphicalDisplay =
  let
    getLinearDisplay name n =
      let selectorFor i = fold [".", name, " .cell", show i]
      in LinearComponent <$> traverse elementsBySelector (selectorFor <$> 0..n)
    getSenaryDisplay name =
      let selectorFor p i = fold [".", name, "-", p, " .cell", show i]
      in SenaryComponent
        <$> traverse elementsBySelector (selectorFor "units" <$> 0..5)
        <*> traverse elementsBySelector (selectorFor "sixes" <$> 0..5)
  in ado
    season     <- getLinearDisplay "season"       6
    month      <- getLinearDisplay "month"        12
    week       <- getLinearDisplay "week"         60
    dayOfMonth <- getLinearDisplay "day-of-month" 30
    mythRole   <- getLinearDisplay "myth-role"    9
    mythNumber <- getLinearDisplay "myth-number"  40
    hour       <- getLinearDisplay "hours-ring"   10
    minute     <- getSenaryDisplay "minute"
    second     <- getSenaryDisplay "second"
    subsecond  <- getSenaryDisplay "subsecond"
    in { season, month, week, dayOfMonth, mythRole, mythNumber,
         hour, minute, second, subsecond, year: None, dayOfYear: None }

setGraphicalDisplay :: HoneyDate -> GraphicalDisplay -> Effect Unit
setGraphicalDisplay date display =
  set _.year *> set _.season *> set _.month *> set _.week *> set _.dayOfYear *>
  set _.dayOfMonth *> set _.mythRole *> set _.mythNumber *>
  set _.hour *> set _.minute *> set _.second *> set _.subsecond
  where
    set :: (forall a. HoneyComponents a -> a) -> Effect Unit
    set getFrom = setComponent (getFrom display) (getFrom date)

    setComponent :: DisplayComponent -> Int -> Effect Unit
    setComponent None _ = mempty
    setComponent (LinearComponent c) n = setElements c n
    setComponent (SenaryComponent units sixes) n =
      setElements units (mod n 6) *> setElements sixes (n/6)

    setElements :: Array (Array Element) -> Int -> Effect Unit
    setElements elements digit = do
      foldMap (foldMap $ removeClass "active") elements
      foldMap (foldMap $ removeClass "filled") elements
      foldMap (addClass "active") (fold $ elements !! digit)
      foldMap (foldMap $ addClass "filled") (take digit $ fromArray elements)

displayDate :: JSDate -> TextualDisplay -> GraphicalDisplay -> Effect Unit
displayDate date t g =
  let honeyDate = gregorianToHoney date
  in setTextualDisplay honeyDate t *> setGraphicalDisplay honeyDate g

displayNow :: TextualDisplay -> GraphicalDisplay -> Effect Unit
displayNow t g = do
  n <- now
  displayDate n t g

setup :: Effect Unit
setup = void do
  t <- getTextualDisplay
  g <- getGraphicalDisplay
  setInterval (floor honeyDurations.subsecond) (displayNow t g)

main :: Effect Unit
main = mempty

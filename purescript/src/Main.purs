module Main where

import Prelude

import Data.Array ((..), (!!), drop, take)
import Data.Char (toCharCode)
import Data.Enum (fromEnum)
import Data.Filterable (filterMap)
import Data.Foldable (fold)
import Data.Int (floor, radix, toStringAs)
import Data.JSDate (JSDate, getTime, jsdate, now)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, maybe)
import Data.String (codePointAt, length)
import Data.String.CodeUnits (singleton)
import Data.Traversable (for_, traverse, traverse_)
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
  | SimpleComponent (Array Element)
  | ComplexComponent (Array (Array Element))

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

elementsBySelector :: String -> Effect (Array Element)
elementsBySelector selector = do
  w <- window
  d <- document w
  r <- querySelectorAll (QuerySelector selector) (toParentNode $ toDocument d)
  filterMap fromNode <$> toArray r

elementById :: String -> Effect (Maybe Element)
elementById = map (_ !! 0) <<< elementsBySelector <<< ("#" <> _)

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
    _.season     `with` \s -> maybe "" (_ <> " Season") (seasons !! s)
    _.month      `with` show
    _.dayOfMonth `with` \d -> maybe "" (_.sajemTan) (letterCycle !! d)
    _.mythRole   `with` \m -> maybe "" (_.sajemTan) (mythCycle !! m)
    _.mythNumber `with` show
    _.hour       `with` show
    _.minute     `with` toSenary 2
    _.second     `with` toSenary 2
    _.subsecond  `with` toSenary 2
    in unit

getGraphicalDisplay :: Effect GraphicalDisplay
getGraphicalDisplay =
  let
    getSenaryDisplay unit =
      ComplexComponent <$> traverse elementsBySelector do
        place <- ["units", "sixes"]
        digit <- map show (0..5)
        pure $ fold [".", unit, "-", place, " .cell", digit]
    getHoursDisplay =
      let getSector i = elementsBySelector (".hours-ring .sector" <> show i)
      in ComplexComponent <$> traverse getSector (0..9)
  in ado
    year       <- pure None
    season     <- pure None
    month      <- pure None
    week       <- pure None
    dayOfYear  <- pure None
    dayOfMonth <- pure None
    mythRole   <- pure None
    mythNumber <- pure None
    hour       <- getHoursDisplay
    minute     <- getSenaryDisplay "minute"
    second     <- getSenaryDisplay "second"
    subsecond  <- getSenaryDisplay "subsecond"
    in { year, season, month, week, dayOfYear, dayOfMonth, mythRole, mythNumber,
         hour, minute, second, subsecond }

addClass :: String -> Element -> Effect Unit
addClass c e = classList e >>= flip TL.add c

removeClass :: String -> Element -> Effect Unit
removeClass c e = classList e >>= flip TL.remove c

setGraphicalDisplay :: HoneyDate -> GraphicalDisplay -> Effect Unit
setGraphicalDisplay date display =
  let
    removeClasses cs = for_ cs <<< flip removeClass
    codePointToInt c = fromEnum c - toCharCode '0'
    getDigit i s = fromMaybe 0 (map codePointToInt $ codePointAt i s)
    setSenaryComponent :: (forall a. HoneyComponents a -> a) -> Effect Unit
    setSenaryComponent getFrom =
      let
        senaryString = toSenary 2 (getFrom date)
        units = getDigit 1 senaryString
        sixes = getDigit 0 senaryString
        elements = unsafePartial (\(ComplexComponent x) -> x) $ getFrom display
        elementsForDigit i = fromMaybe [] (elements !! i)
      in do
        traverse_ (traverse_ $ removeClasses ["filled", "active"]) elements
        traverse_ (addClass "active") (elementsForDigit units)
        traverse_ (addClass "active") (elementsForDigit (6+sixes))
        traverse_ (traverse_ $ addClass "filled") (take units elements)
        traverse_ (traverse_ $ addClass "filled") (take sixes (drop 6 elements))
    setDecimalComponent :: (forall a. HoneyComponents a -> a) -> Effect Unit
    setDecimalComponent getFrom =
      let
        digit = getDigit 0 (show (getFrom date))
        elements = unsafePartial (\(ComplexComponent x) -> x) $ getFrom display
      in do
        traverse_ (traverse_ $ removeClasses ["filled", "active"]) elements
        traverse_ (addClass "active") (fromMaybe [] (elements !! digit))
        traverse_ (traverse_ $ addClass "filled") (take digit elements)
  in ado
    setDecimalComponent _.hour
    setSenaryComponent _.minute
    setSenaryComponent _.second
    setSenaryComponent _.subsecond
    in unit

displayDate :: JSDate -> TextualDisplay -> GraphicalDisplay -> Effect Unit
displayDate date td gd = do
  let honeyDate = gregorianToHoney date
  setTextualDisplay honeyDate td
  setGraphicalDisplay honeyDate gd

displayNow :: TextualDisplay -> GraphicalDisplay -> Effect Unit
displayNow td gd = do
  n <- now
  displayDate n td gd

setup :: Effect Unit
setup = void do
  t <- getTextualDisplay
  g <- getGraphicalDisplay
  setInterval (floor honeyDurations.subsecond) (displayNow t g)

main :: Effect Unit
main = mempty

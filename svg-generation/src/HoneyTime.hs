{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module HoneyTime (clockDial, dateDial, mythDial, hexBackground, svg) where

import Data.Convertible (convert)
import Data.Foldable (fold, foldMap)
import Data.Maybe (fromMaybe)
import Data.Prizm.Color (BlendableColor, ColorCoord(..), HexRGB, RGB, interpolate, mkRGB, unHexRGB)
import Data.Prizm.Color.CIE as CIE
import Graphics.Svg
import qualified Data.Text as T

instance BlendableColor CIE.LAB where
  interpolate w (CIE.unLAB -> ColorCoord(al, aa, ab),
      CIE.unLAB -> ColorCoord(bl, ba, bb)) =
    let
      pctClamp i = max (min i 100) (-100)
      pct i = fromIntegral (pctClamp i) / 100
      w' = pct w
      ColorCoord(nl, na, nb) = (* w') <$> ColorCoord(bl - al, ba - aa, bb - ab)
    in CIE.mkLAB (al + nl) (aa + na) (ab + nb)

dup :: (a -> a -> b) -> a -> b
dup f x = f x x

tshow :: Show a => a -> T.Text
tshow = T.pack . show

degrees :: RealFloat a => a -> a
degrees x = x * 180 / pi

radians :: RealFloat a => a -> a
radians x = x * pi / 180

polar :: RealFloat a => (a -> a -> b) -> a -> a -> b
polar fn r theta = fn (r * cos theta) (r * sin theta)

apothem :: (Integral a, RealFloat b) => a -> b -> b
apothem n radius = radius * cos (pi / fromIntegral n)

polygonAngle :: (Integral a, RealFloat b) => a -> Bool -> a -> b
polygonAngle n vertexAtTop i =
  let
    sides  = fromIntegral n
    offset = if vertexAtTop then 0 else pi/sides
  in fromIntegral i * 2*pi/sides - pi/2 + offset

hexagonAngle :: (Integral a, RealFloat b) => Bool -> a -> b
hexagonAngle = polygonAngle 6

point :: RealFloat a => a -> a -> T.Text
point x y = T.concat [toText x, " ", toText y, " "]

arc :: RealFloat a => a -> a -> a -> T.Text
arc targetAngle radius rotation = T.intercalate " " [
  "A",                                -- command for arc with absolute values
  toText radius,                      -- x-radius
  toText radius,                      -- y-radius
  toText (abs rotation),              -- x-axis rotation
  "0",                                -- large arc flag
  if rotation > 0 then "1" else "0",  -- sweep direction flag
  polar point radius targetAngle]     -- target coordinate

polygon :: (Integral a, RealFloat b) => a -> b -> Bool -> Element
polygon n r vertexAtTop =
  let
    element p    = polygon_ [Points_ <<- p]
    calcVertex i = polar point r (polygonAngle n vertexAtTop i)
  in element $ foldMap calcVertex [0..(n-1)]

hexagon :: RealFloat a => a -> Bool -> Element
hexagon = polygon 6

annulusSector :: (Integral a, RealFloat b) => a -> b -> b -> Bool -> Element
annulusSector n outerRadius innerRadius innerArc =
  let
    theta = 2 * pi / fromIntegral n
    start = polygonAngle n True 0
    end   = polygonAngle n True 1
  in path_ [
    D_ <<- fold [
      polar mA outerRadius start,            -- move to start position
      arc end outerRadius theta,             -- draw outer arc
      polar lA innerRadius end,              -- draw straight line to inner arc
      if innerArc                            -- draw inner arc / line
         then arc start innerRadius (-theta)
         else polar lA innerRadius start,
      z]]                                    -- draw straight line back to start

createRing :: (Integral a, Show a, RealFloat b)
           => a -> b -> b -> T.Text -> Bool -> Element
createRing n outerRadius innerRadius className innerArcs =
  let
    useId = className <> "-template"
    template = defs_ [] $
      with (annulusSector n outerRadius innerRadius innerArcs) [Id_ <<- useId]
    createUsage i = use_ [
      XlinkHref_ <<- "#" <> useId,
      Class_     <<- "cell cell" <> tshow i,
      Transform_ <<- rotate (fromIntegral i * 360 / fromIntegral n)]
  in g_ [Class_ <<- className] (template <> foldMap createUsage [0..(n-1)])

ringDotMarker :: (Integral a, RealFloat b) => a -> b -> a -> Element
ringDotMarker n radius i =
  foldMap (\n -> dot radius (start + fromIntegral n * spacing)) [0..i-1]
  where
    spacing = pi / 60
    start = polygonAngle n False i - spacing * (fromIntegral i - 1) / 2.0
    dot = polar $ \x y -> circle_ [
        R_  <<- "1.25",
        Cx_ <<- toText x,
        Cy_ <<- toText y]

hexagonSector :: RealFloat b => b -> b -> Element
hexagonSector apo innerRadius =
  let
    start = hexagonAngle True 0
    end = hexagonAngle True 1
    circumradius = apo / cos (pi/6)
  in path_ [
    D_ <<- fold [
      polar mA apo start,                    -- move to start position
      polar lA circumradius ((start+end)/2), -- draw line to outermost point
      polar lA apo end,                      -- draw line to far edge
      polar lA innerRadius end,              -- draw line to inner edge
      polar lA innerRadius start,            -- draw inside line
      z]]                                    -- draw line back to start

createHexagonRing :: RealFloat b => b -> b -> T.Text -> Element
createHexagonRing apo innerRadius className =
  let
    useId = className <> "-template"
    template = defs_ [] $ with (hexagonSector apo innerRadius) [Id_ <<- useId]
    createUsage i = use_ [
      XlinkHref_ <<- "#" <> useId,
      Class_     <<- "cell cell" <> tshow i,
      Transform_ <<- rotate (fromIntegral i * 360 / 6)]
  in g_ [Class_ <<- className] (template <> foldMap createUsage [0..5])

translateHelper :: (Integral a, RealFloat b)
                   => a -> b -> b -> Bool -> a -> T.Text
translateHelper n r m vertexAtTop i =
  let multiplier = if vertexAtTop then r else apothem n r
  in polar translate (m * multiplier) (polygonAngle n vertexAtTop i)

hexTranslateHelper :: (RealFloat a, Integral b) => a -> a -> Bool -> b -> T.Text
hexTranslateHelper = translateHelper 6

floretDotMarker :: Integral a => a -> Element
floretDotMarker n = g_ [] $ foldMap (dot distance . angle) [0..n-1]
  where
    distance = 5 - 5 / fromIntegral n
    angle = polygonAngle n True
    dot = polar $ \x y -> circle_ [
        R_  <<- "1.25",
        Cx_ <<- toText x,
        Cy_ <<- toText y]

hexagonFloret :: RealFloat a => T.Text -> a -> T.Text -> Element
hexagonFloret name tileRadius useId = g_ [Class_ <<- name] $ foldMap cell [0..5]
  where
    cell :: (Show i, Integral i) => i -> Element
    cell i = g_ [Transform_ <<- hexTranslateHelper tileRadius 2 False i]
      $ fold [
        use_ [XlinkHref_ <<- useId, Class_ <<- "cell cell" <> tshow i],
        floretDotMarker i]

innerClockDial :: RealFloat a => a -> T.Text -> Element
innerClockDial tileRadius useId =
  let
    group = g_ [Class_ <<- "inner-clock-dial"]
    florets = do
      unit  <- ["subsecond", "second", "minute"]
      place <- ["units", "sixes"]
      return $ T.concat [unit, "-", place]
    createFloret (i, name) = hexagonFloret name tileRadius useId
      `with` [Transform_ <<- hexTranslateHelper tileRadius 8 False i]
  in group $ foldMap createFloret (zip [0..] florets)

outerClockDial :: RealFloat a => a -> Element
outerClockDial tileRadius =
  createRing 10 (tileRadius * 11) (tileRadius * 10) "hours-ring" True
      <> foldMap (ringDotMarker 10 (tileRadius * 10.5)) [0..9]

sunMoonDial :: RealFloat a => a -> Element
sunMoonDial tileRadius =
  let radius = 4.5 * apothem 6 tileRadius
  in defs_ [] (
    clipPath_ [Id_ <<- "sun-moon-clip"] (
      circle_ [R_ <<- toText radius])
    <> radialGradient_ [Id_ <<- "cutaway-shadow", Cy_ <<- "52%"] (
      stop_ [Offset_ <<- "0.88", Stop_color_ <<- "rgba(0, 0, 0, 0)"]
      <> stop_ [Offset_ <<- "1", Stop_color_ <<- "rgba(0, 0, 0, 0.6)"]))
  <> g_ [Clip_path_ <<- "url(#sun-moon-clip)"] (
    use_ [
      Class_     <<- "sun-moon-dial",
      Width_     <<- toText (radius * 2 * 3) <> "px",
      Height_    <<- toText (radius * 2) <> "px",
      XlinkHref_ <<- "#sun-moon-plate",
      Transform_ <<- translate (-radius) (-radius)]
    <> circle_ [
      R_ <<- toText radius,
      Fill_ <<- "url(#cutaway-shadow)"])

clockDialDecoration :: RealFloat a => a -> T.Text -> Element
clockDialDecoration tileRadius useId =
  let
    group    = g_ [Class_ <<- "clock-decoration"]
    isEven n = even n
    angleAt  = hexagonAngle True
    withThickStroke = flip with [
      Stroke_          <<- "black",
      Stroke_width_    <<- tshow 3.5,
      Stroke_linecap_  <<- "round",
      Stroke_linejoin_ <<- "round"]
    createSpoke i = fold [
      withThickStroke $ path_ [
        D_ <<- fold (
          if even i
          then [
            polar mA (tileRadius * 5) (angleAt i - 0.055),
            polar lA (tileRadius * 9) (angleAt i - 0.03),
            polar mA (tileRadius * 5) (angleAt i + 0.055),
            polar lA (tileRadius * 9) (angleAt i + 0.03),
            z]
          else [
            polar mA (tileRadius * 5) (angleAt i),
            polar lA (tileRadius * 9) (angleAt i),
            z])]]
  in group (foldMap createSpoke [0..5])

clockDial :: RealFloat a => a -> Element
clockDial tileRadius =
  let
    tileId = "#hex-tile"
    useId = "#clock-dial"
    mainGroup = g_
      [Id_ <<- T.tail useId, Class_ <<- "clock-dial"]
      (fold [
        circle_ [Cx_ <<- "0", Cy_ <<- "0", R_ <<- toText (1 + tileRadius * 11),
          Fill_ <<- "none", Stroke_width_ <<- "3", Stroke_ <<- "black"],
        defs_ [] (hexagon tileRadius True `with` [Id_ <<- T.tail tileId]),
        clockDialDecoration tileRadius tileId,
        sunMoonDial tileRadius,
        innerClockDial tileRadius tileId,
        outerClockDial tileRadius])
  in mainGroup

glyphBaseTransform :: RealFloat a => a -> a -> T.Text
glyphBaseTransform nativeSize displaySize =
  let
    scaleFactor = displaySize / nativeSize
    translateFactor = -nativeSize / 2
  in rotate 90 <> dup scale scaleFactor <> dup translate translateFactor

glyphRing :: (Integral a, Show a, RealFloat b)
          => T.Text -> T.Text -> a -> b -> b -> b -> Element
glyphRing useIdPrefix className n tileRadius size d =
  let
    createUsage i = use_ [
      XlinkHref_ <<- "#" <> useIdPrefix <> tshow i,
      Class_     <<- "cell cell" <> tshow i,
      Transform_ <<-
        translateHelper n tileRadius d False (fromIntegral i)
        <> rotate (degrees $ polygonAngle n False (fromIntegral i))
        <> glyphBaseTransform 100 (tileRadius * size)]
  in g_ [Class_ <<- className] $ foldMap createUsage [0..(n-1)]

mythDial :: RealFloat a => a -> Element
mythDial tileRadius =
  let
    innerDivide  = tileRadius * 3
    middleDivide = tileRadius * 8.5
    dialSize     = tileRadius * 11
    numberGlyphs = glyphRing "honey" "myth-number" 40 tileRadius 2.1 9.96
    roleGlyphs   = glyphRing "mythrole" "myth-role" 9 tileRadius 2.5 6.5
  in g_ [Class_ <<- "myth-dial"] $ fold [
    circle_ [Cx_ <<- "0", Cy_ <<- "0", R_ <<- toText (1 + dialSize),
      Fill_ <<- "none", Stroke_width_ <<- "3", Stroke_ <<- "black"],
    createRing 9 middleDivide (innerDivide + 5) "myth-role" False,
    with (polygon 9 (innerDivide + tileRadius/2) True)
      [Fill_ <<- "none", Stroke_ <<- "black", Stroke_width_ <<- "2"],
    roleGlyphs,
    numberGlyphs,
    use_ [
      XlinkHref_ <<- "#honey0",
      Class_     <<- "year",
      Fill_      <<- "none",
      Transform_ <<-
        translate 0 (-1)
        <> rotate 90
        <> glyphBaseTransform 100 (tileRadius * 3.5)]]

createDotMarkers :: (Integral a, RealFloat b)
                 => a -> (a -> Bool) -> b -> b -> T.Text -> Element
createDotMarkers n skip radius dotRadius className =
  let
    useId = className <> "-template"
    circle = polar (\x y -> circle_ [Cx_ <<- toText x, Cy_ <<- toText y])
    template = defs_ [] $ with (circle radius (-pi/2)) [
      Id_ <<- useId,
      R_ <<- toText dotRadius]
    createUsage (i, x) = use_ [
      XlinkHref_ <<- "#" <> useId,
      Class_     <<- "cell cell" <> tshow i,
      Transform_ <<- rotate (fromIntegral x * 360 / fromIntegral n)]
  in g_ [Class_ <<- className] $
    template <> foldMap createUsage (zip [0..] $ filter skip [0..(n-1)])

dateDial :: RealFloat a => a -> Element
dateDial tileRadius =
  let
    divide0      = tileRadius * 1.5
    divide1      = tileRadius * 4.75
    divide2      = tileRadius * 8.5
    divide3      = tileRadius * 9
    dialSize     = tileRadius * 11
    seasonGlyphs = glyphRing "season" "season" 6 tileRadius 1.8 3.9
    monthGlyphs  = glyphRing "honey" "month" 12 tileRadius 1.9 6.85
    dayGlyphs    = glyphRing "letter" "day-of-month" 30 tileRadius 1.1 10.11
  in g_ [Class_ <<- "date-dial"] $ fold [
    circle_ [Cx_ <<- "0", Cy_ <<- "0", R_ <<- toText (1 + dialSize),
      Fill_ <<- "none", Stroke_width_ <<- "3", Stroke_ <<- "black"],
    createRing 30 dialSize divide3 "day-of-month" True,
    createRing 12 divide3 divide1 "month" True,
    createDotMarkers 72 (\x -> x `mod` 6 /= 0) divide2 2 "week",
    createHexagonRing divide1 divide0 "season",
    seasonGlyphs,
    monthGlyphs,
    dayGlyphs]

hexGridTranslation :: (Integral a, RealFloat b) => a -> a -> b -> T.Text
hexGridTranslation x y r = translate
  (fromIntegral (x*2 + mod y 2) * apothem 6 r)
  (fromIntegral y * r * 1.5 + r * 0.4)

toLAB :: RGB -> CIE.LAB
toLAB = convert

toHex :: CIE.LAB -> HexRGB
toHex = convert

darken :: CIE.LAB -> CIE.LAB
darken lch =
  let ColorCoord (l, c, h) = unLAB lch
  in mkLAB (l-10) c h

getCellColor :: Integral a => (CIE.LAB, CIE.LAB) -> (a, a) -> (a, a) -> T.Text
getCellColor colorRange (maxX, maxY) (x, y) =
  let
    relativeX = fromIntegral x / fromIntegral maxX
    relativeY = fromIntegral y / fromIntegral maxY
    percent = round $ sqrt ((0.5 - relativeX)**2 + (0.9 - relativeY)**2) * 100
    interpolation = interpolate percent colorRange
    result = if (x - (y `mod` 2)) `mod` 3 == 0
      then darken interpolation
      else interpolation
  in unHexRGB (toHex result)

generateStyles :: (Integral a, Show a) => (a, a) -> (a, a) -> Element
generateStyles (maxX, maxY) (x, y) =
  let
    percentY = round $ ((fromIntegral (x+y) / fromIntegral (maxX+maxY))::Float) * 100
    tileClass = ".tile-" <> tshow x <> "-" <> tshow y
    themeRanges = [
      (toLAB $ mkRGB 200 200 200, toLAB $ mkRGB 200 200 200),  -- No theme
      (toLAB $ mkRGB 130 165 200, toLAB $ mkRGB 129  92 140),  -- Thunder
      (toLAB $ mkRGB 155 155 155, toLAB $ mkRGB 113 143 200),  -- Rain
      (toLAB $ mkRGB 255 183   0, toLAB $ mkRGB 191 113 191),  -- Flower
      (toLAB $ mkRGB  46  53 184, toLAB $ mkRGB  15 150 110),  -- River
      (toLAB $ mkRGB 153 138 158, toLAB $ mkRGB 161 112 138),  -- Rock
      (toLAB $ mkRGB 190   0   0, toLAB $ mkRGB  45  45  45),  -- Spider
      (toLAB $ mkRGB 255 183   0, toLAB $ mkRGB 219 106   0),  -- Bee
      (toLAB $ mkRGB 179  98  12, toLAB $ mkRGB  45  66  12),  -- Bear
      (toLAB $ mkRGB 235  52 125, toLAB $ mkRGB  19  86 212)]  -- Bird
  in toElement $
    tileClass <> " {"
      <> "transition: fill 5s ease " <> tshow (fromIntegral y / 5.0) <> "s;"
      <> "} "
    <> foldMap
      (\(i, range) -> ".theme-" <> tshow i <> " " <> tileClass <> " {"
        <> "fill: " <> getCellColor range (maxX, maxY) (x, y) <> ";"
        <> "} ")
      (zip [0..] themeRanges)

hexBackground :: (Integral a, Show a) => a -> Element
hexBackground widthHeight =
  let
    tileId = "#background-tile"
    hexRadius = 20
    viewBoxSize = 2 * apothem 6 hexRadius * fromIntegral widthHeight
    defs = defs_ [] (hexagon hexRadius True `with` [Id_ <<- T.tail tileId])
    coords = [(x, y) | x <- [0..widthHeight], y <- [0..widthHeight+1]]
    makeTile (x, y) = use_ [
        XlinkHref_    <<- tileId,
        Class_        <<- "tile-" <> tshow x <> "-" <> tshow y,
        Transform_    <<- hexGridTranslation x y hexRadius]
    makeOutline (x, y) = use_ [
        XlinkHref_    <<- tileId,
        Fill_         <<- "none",
        Transform_    <<- hexGridTranslation x y hexRadius,
        Stroke_       <<- "black"]
  in doctype
    <> (\x y -> with (svg11_ y) x) [
        Version_ <<- "1.1",
        Class_   <<- "hex-background",
        Width_   <<- tshow viewBoxSize,
        Height_  <<- tshow viewBoxSize,
        ViewBox_ <<- dup point 0 <> dup point viewBoxSize]
      (defs
        <> style_ [] (foldMap (generateStyles (widthHeight, widthHeight+1)) coords)
        <> g_ [Class_ <<- "background-cells"] (foldMap makeTile coords)
        <> g_ [Class_ <<- "background-mesh"] (foldMap makeOutline coords))

svg :: T.Text -> Element -> Element
svg className content =
  let
    tileRadius  = 12
    widthHeight = tileRadius * 12 * 2
    topLeft     = widthHeight / (-2)
  in
    doctype
    <> with (svg11_ content) [
      Version_ <<- "1.1",
      Class_   <<- className,
      Width_   <<- tshow widthHeight,
      Height_  <<- tshow widthHeight,
      ViewBox_ <<- dup point topLeft <> dup point widthHeight]

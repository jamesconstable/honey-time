module Confetti (module OptExports, ConfettiOptions, angle, colors, confetti,
    decay, disableForReducedMotion, drift, gravity, origin, particleCount,
    scalar, shapes, spread, startVelocity, ticks, zIndex) where

import Data.Options ((:=)) as OptExports
import Data.Options (Option, Options, opt, options)
import Effect (Effect)
import Foreign (Foreign)
import Prelude (Unit)

foreign import confettiImpl :: Foreign -> Effect Unit

data ConfettiOptions

particleCount :: Option ConfettiOptions Int
particleCount = opt "particleCount"

angle :: Option ConfettiOptions Number
angle = opt "angle"

spread :: Option ConfettiOptions Number
spread = opt "spread"

startVelocity :: Option ConfettiOptions Number
startVelocity = opt "startVelocity"

decay :: Option ConfettiOptions Number
decay = opt "decay"

gravity :: Option ConfettiOptions Number
gravity = opt "gravity"

drift :: Option ConfettiOptions Number
drift = opt "drift"

ticks :: Option ConfettiOptions Number
ticks = opt "ticks"

origin :: Option ConfettiOptions { x :: Number, y :: Number }
origin = opt "origin"

colors :: Option ConfettiOptions (Array String)
colors = opt "colors"

shapes :: Option ConfettiOptions (Array String)
shapes = opt "shapes"

scalar :: Option ConfettiOptions Number
scalar = opt "scalar"

zIndex :: Option ConfettiOptions Int
zIndex = opt "zIndex"

disableForReducedMotion :: Option ConfettiOptions Boolean
disableForReducedMotion = opt "disableForReducedMotion"

confetti :: Options ConfettiOptions -> Effect Unit
confetti opts = confettiImpl (options opts)

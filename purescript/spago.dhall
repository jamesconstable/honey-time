{ name = "honey-time"
, dependencies =
  [ "array-views"
  , "console"
  , "datetime"
  , "effect"
  , "filterable"
  , "js-timers"
  , "lists"
  , "options"
  , "psci-support"
  , "web-dom"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}

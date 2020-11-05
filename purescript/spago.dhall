{ name = "honey-time"
, dependencies =
  [ "console"
  , "datetime"
  , "effect"
  , "filterable"
  , "js-timers"
  , "lists"
  , "psci-support"
  , "web-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}

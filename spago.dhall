{ name = "monielovebot"
, dependencies =
  [ "console"
  , "datetime"
  , "effect"
  , "formatters"
  , "kishimen"
  , "node-fs"
  , "node-telegram-bot-api"
  , "numbers"
  , "psci-support"
  , "simple-json"
  , "simple-timestamp"
  , "string-parsers"
  , "stringutils"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}

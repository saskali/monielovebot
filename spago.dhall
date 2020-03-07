{ name = "monielovebot"
, dependencies =
  [ "console"
  , "effect"
  , "kishimen"
  , "node-fs"
  , "node-telegram-bot-api"
  , "numbers"
  , "psci-support"
  , "simple-json"
  , "string-parsers"
  , "stringutils"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}

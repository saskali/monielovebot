{ name = "moniebot"
, dependencies =
    [ "console"
    , "effect"
    , "node-fs"
    , "node-telegram-bot-api"
    , "numbers"
    , "psci-support"
    , "simple-json"
    , "string-parsers"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}

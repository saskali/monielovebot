{ name = "moniebot"
, dependencies =
    [ "console"
    , "effect"
    , "node-fs"
    , "node-telegram-bot-api"
    , "psci-support"
    , "simple-json"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}

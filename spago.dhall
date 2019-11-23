{ name = "moniebot"
, dependencies =
    [ "console"
    , "effect"
    , "node-telegram-bot-api"
    , "psci-support"
    , "simple-json"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}

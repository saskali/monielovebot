let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.4-20191110/packages.dhall sha256:563a7f694e18e6399f7f6d01f5b7e3c3345781655d99945768f48e458feb93a4

let overrides = {=}

let additions = { node-telegram-bot-api = upstream.node-telegram-bot-api // { repo = "https://github.com/f-f/purescript-node-telegram-bot-api.git", version = "76e02478f3a4ab76fcdc8438d5721e278b2f03db" } }

in  upstream // overrides // additions

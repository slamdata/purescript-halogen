{ name = "halogen"
, license = "Apache-2.0"
, repository = "https://github.com/purescript-halogen/purescript-halogen"
, dependencies =
    [ "aff"
    , "avar"
    , "console"
    , "const"
    , "dom-indexed"
    , "event"
    , "effect"
    , "foreign"
    , "fork"
    , "free"
    , "freeap"
    , "halogen-vdom"
    , "media-types"
    , "nullable"
    , "ordered-collections"
    , "parallel"
    , "profunctor"
    , "transformers"
    , "unsafe-coerce"
    , "unsafe-reference"
    , "web-file"
    , "web-uievents"
    , "debug"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}

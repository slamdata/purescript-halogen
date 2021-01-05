let config = ../../spago.dhall

in config // {
  sources = config.sources # [ "examples/portal/**/*.purs" ],
  dependencies = config.dependencies
}

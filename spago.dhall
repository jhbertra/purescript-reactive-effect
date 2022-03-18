{ name = "frp"
, license = "MIT"
, repository = "https://github.com/jhbertra/purescript-frp.git"
, dependencies =
  [ "either"
  , "filterable"
  , "foldable-traversable"
  , "maybe"
  , "prelude"
  , "these"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}

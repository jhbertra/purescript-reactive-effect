{ name = "frp"
, license = "MIT"
, repository = "https://github.com/jhbertra/purescript-frp.git"
, dependencies =
  [ "aff", "effect", "either", "lists", "maybe", "prelude", "tuples" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}

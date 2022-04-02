{ name = "frp"
, license = "MIT"
, repository = "https://github.com/jhbertra/purescript-frp.git"
, dependencies =
  [ "aff"
  , "arrays"
  , "control"
  , "debug"
  , "effect"
  , "either"
  , "enums"
  , "exceptions"
  , "exists"
  , "filterable"
  , "foldable-traversable"
  , "gen"
  , "identity"
  , "lazy"
  , "lists"
  , "maybe"
  , "newtype"
  , "prelude"
  , "profunctor"
  , "quickcheck"
  , "quickcheck-laws"
  , "record"
  , "refs"
  , "safe-coerce"
  , "spec"
  , "spec-quickcheck"
  , "tailrec"
  , "these"
  , "transformers"
  , "tuples"
  , "unfoldable"
  , "unlift"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

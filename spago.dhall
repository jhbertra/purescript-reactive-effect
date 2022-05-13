{ name = "reactive-effect"
, license = "MIT"
, repository = "https://github.com/jhbertra/purescript-reactive-effect.git"
, dependencies =
  [ "aff"
  , "arrays"
  , "bifunctors"
  , "catenable-lists"
  , "concurrent-queues"
  , "control"
  , "datetime"
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
  , "integers"
  , "lazy"
  , "lcg"
  , "lists"
  , "maybe"
  , "monad-control"
  , "newtype"
  , "nonempty"
  , "orders"
  , "parallel"
  , "partial"
  , "prelude"
  , "quickcheck"
  , "quickcheck-laws"
  , "record"
  , "refs"
  , "safe-coerce"
  , "spec"
  , "spec-quickcheck"
  , "strings"
  , "tailrec"
  , "these"
  , "transformers"
  , "tuples"
  , "unfoldable"
  , "unlift"
  , "unsafe-coerce"
  , "web-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

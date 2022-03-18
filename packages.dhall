let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190626/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.7-20220303/packages.dhall sha256:d7cbc15ea16768e4a4f99baa58a54559dd2648c6c1362de2469d9e41c23b28c3

in  upstream
  with unlift =
      mkPackage
        [ "aff"
        , "effect"
        , "either"
        , "identity"
        , "lists"
        , "maybe"
        , "monad-control"
        , "prelude"
        , "transformers"
        , "tuples"
        ]
        "https://github.com/tweag/purescript-unlift"
        "v1.0.1"

module Main where
import Test.DocTest
main = doctest [
      "-ilib"
    , "lib/ByOtherNames.hs"
    , "lib/ByOtherNames/Aeson.hs"
    , "lib/ByOtherNames/TH.hs"
    , "lib/ByOtherNames/Constraint.hs"
    , "lib/ByOtherNamesH.hs"
    , "lib/ByOtherNamesH/Aeson.hs"
    ]
import Test.DocTest
main :: IO ()
main = doctest
    [ "-isrc"
    , "-optP-include"
    , "optPdist/build/autogen/cabal_macros.h"
    , "src/Text/Blaze/JSON.hs"
    ]

import Test.DocTest
main :: IO ()
main = doctest ["-isrc", "src/Text/Blaze/JSON.hs"]

import Control.Monad

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Data.Aeson as A
import Data.Scientific
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H

import qualified Text.Blaze.JSON as B

fromAeson :: A.Value -> B.JSON
fromAeson (A.Object o) = B.object . map (fmap fromAeson) $ H.toList o
fromAeson (A.Array  a) = B.array $ fmap fromAeson a
fromAeson (A.String t) = B.text t
fromAeson (A.Number n) = either B.double B.integral (floatingOrInteger n :: Either Double Int)
fromAeson (A.Bool   b) = B.bool b
fromAeson A.Null       = B.null

text :: Gen T.Text
text = sized $ \n -> do
    k <- choose (0, n)
    s <- sequence
        [ oneof
            [ oneof $ map return "\\\""
            , oneof $ map return ['\0' .. '\x20']
            , toEnum `fmap` choose (0, 0xffff)
            ]
        | _ <- [1 .. k]
        ]
    return $ T.pack s

scolar :: Gen A.Value
scolar = oneof
     [ return A.Null
    , A.Bool `fmap` arbitrary
    , (A.Number . fromIntegral) `fmap` (arbitrary :: Gen Int)
    , (A.Number . fromFloatDigits) `fmap` (arbitrary :: Gen Double)
    , fmap A.String text
    ]

arbit :: Int -> Gen A.Value
arbit 0 = scolar
arbit d = oneof
    [ scolar
    , sized $ \l ->
        fmap (A.Array . V.fromList) $ vectorOf l (arbit $ d-1)
    , sized $ \l ->
        fmap (A.Object . H.fromList) $ vectorOf l ((,) `fmap` text `ap` arbit (d-1))
    ]

instance Arbitrary A.Value where
    arbitrary = arbit 2

main :: IO ()
main = defaultMain $ testGroup ""
    [ testProperty "same to aeson" $ \value ->
        A.encode value === B.encode (fromAeson value)
    ]

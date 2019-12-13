import Control.Monad
import Test.QuickCheck

data Point a = Point a a
               deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (Point a) where
    arbitrary = liftM2 Point arbitrary arbitrary
    -- TODO: provide a body for shrink
    shrink (Point a b) = shrink a >>= \i -> shrink b >>= \j -> pure (Point i j)

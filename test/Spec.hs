import           Data.Foldable
import           Data.Reduce
import           Test.QuickCheck
import           Data.Word (Word)

main :: IO ()
main = do
    putStrLn "Testing prop1:"
    quickCheck prop1
    putStrLn "Testing prop2:"
    quickCheck prop2
    putStrLn "Testing prop3:"
    quickCheck prop3
    putStrLn "Testing prop4:"
    quickCheck prop4
    putStrLn "Testing prop5:"
    quickCheck prop5
    putStrLn "Testing prop6:"
    quickCheck prop6

-- Test that reduce returns the same result as fold
prop1 :: [ Int ] -> Bool
prop1 ilst =
    let slst :: [ String ]
        slst = show <$> ilst
    in
    fold slst == reduce slst

-- Test that reduceMap returns the same result as foldMap
prop2 :: [ Int ] -> Bool
prop2 ilst = foldMap show ilst == reduceMap show ilst

-- Test that reduceWith mappend returns the same result as fold
prop3 :: [ Int ] -> Bool
prop3 ilst = 
    let slst :: [ String ]
        slst = show <$> ilst
        expected :: Maybe String
        expected = case slst of
                    [] -> Nothing
                    _  -> Just (fold slst)
    in
    reduceWith mappend slst == expected

-- Check that reduce is a balanced fold
data BalCheck = BalCheck {
    isBalanced :: Bool,
    size :: Int
}

instance Semigroup BalCheck where
    x <> y = BalCheck {
                    isBalanced = isBalanced x
                                    && isBalanced y
                                    && (size x <= (3 * (size y) + 1))
                                    && (size y <= (3 * (size x) + 1)),
                    size = size x + size y + 1
                }

-- Not exactly law-abiding, but we don't care.
instance Monoid BalCheck where
    mempty = BalCheck {
                isBalanced = True,
                size = 0 }

makeChecks :: Word -> [ BalCheck ]
makeChecks wlen =
    let checks :: [ BalCheck ]
        checks = repeat mempty

        len :: Int
        len = fromIntegral (wlen `mod` 1000)
    in
    take len checks

-- Check that reduce is actually balanced
prop4 :: Word -> Bool
prop4 = isBalanced . reduce . makeChecks

-- Check that reduceMap is actually balanced
prop5 :: Word -> Bool
prop5 = isBalanced . reduceMap id . makeChecks

-- Check that reduceWith is actually balanced
prop6 :: Word -> Bool
prop6 len = case (reduceWith mappend . makeChecks $ len) of
                Nothing -> len == 0
                Just r -> isBalanced r

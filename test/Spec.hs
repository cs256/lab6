--------------------------------------------------------------------------------
-- Functional Programming (CS256)                                             --
-- Lab 6: Functors and applicatives                                           --
--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import qualified Lab6 as L

--------------------------------------------------------------------------------

examples :: [(String, Maybe Integer)]
examples =
    [ ("4", Just 4)
    , ("(15 + 16)", Just 31)
    , ("((2 + 4) + (9 + 2))", Just 17)
    , ("(2", Nothing)
    ]

-- | The main entry point to the test suite.
main :: IO ()
main = hspec $ do
    describe "ch" $ do
        it "fails when given the empty list" $
            L.parse (L.ch undefined) "" `shouldBe` Nothing
        it "fails when given an input which does not satisfy the predicate" $
            L.parse (L.ch (=='x')) "zyx" `shouldBe` Nothing
        prop "succeeds when given an input which does satisfy the predicate" $
            \x xs -> L.parse (L.ch (==x)) (x:xs) == Just (x,xs)
    describe "nat" $ do
        prop "parses natural numbers" $ \(Positive n) xs ->
            let suffix = ' ' : xs
            in L.parse L.nat (show n ++ suffix) `shouldBe` Just (n, suffix)
    describe "primitives (lparen, rparen, plus)" $ do
        prop "lparen parses left parentheses" $ \xs ->
            L.parse L.lparen ('(' : xs) `shouldBe` Just ('(', xs)
        prop "rparen parses right parentheses" $ \xs ->
            L.parse L.rparen (')' : xs) `shouldBe` Just (')', xs)
        prop "plus parses +" $ \xs ->
            L.parse L.plus ('+' : xs) `shouldBe` Just ('+', xs)
        prop "lparen fails on other input" $ forAll (except '(') $ \x ->
            L.parse L.lparen [x] `shouldBe` Nothing
        prop "rparen fails on other input" $ forAll (except ')') $ \x ->
            L.parse L.rparen [x] `shouldBe` Nothing
        prop "plus fails on other input" $ forAll (except '+') $ \x ->
            L.parse L.plus [x] `shouldBe` Nothing
    describe "val" $ do
        prop "parses natural numbers with arbitrary amounts of preceding whitespace" $
            \(Positive n) (Positive m) -> L.parse L.val (replicate n ' ' ++ show m) `shouldBe`
                Just (L.Val m, "")
    describe "parseAndEval" $ do
        it "successfully parses the examples from the specification" $
            examples `shouldSatisfy` all (\(xs,r) -> L.parseAndEval xs == r)


except :: Char -> Gen Char
except c = suchThat arbitrary (/= c)

--------------------------------------------------------------------------------

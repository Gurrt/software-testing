import Test.Hspec
import Test.QuickCheck
import Lab2

-- We use HSpec in order to do the testing.
-- In order to run this file, you need to install HSpec first:
--      cabal update
--      cabal install
-- To run the tests:
--      cabal repl week-2
--      (once in GHCI) main
main :: IO ()
main = hspec $ do
    describe "Exercise 1: Recognizing triangles" $ do
        it "recognises equilateral triangles" $
            triangle 3 3 3 `shouldBe` (Equilateral::Shape)

        it "recognises rectangular triangles" $
            triangle 3 4 5 `shouldBe` (Rectangular::Shape)

        it "recognises isosceles triangles" $
            triangle 3 3 2 `shouldBe` (Isosceles::Shape)

        it "recognises other triangles" $
            triangle 2 3 4 `shouldBe` (Other::Shape)

        it "recognises when it is not a triangle" $
            triangle 2 2 5 `shouldBe` (NoTriangle::Shape)

    describe "Exercise 2: Recognizing Permutations" $ do
        it "[3,2,1] is permutation of [1,2,3]" $
            isPermutation [1 :: Integer, 2, 3] [3, 2, 1] `shouldBe` (True :: Bool)

        it "[2,3,1] is a permutation of [1,2,3]" $
            isPermutation [1 :: Integer, 2, 3] [2, 3, 1] `shouldBe` (True :: Bool)

        it "[1,2,3] is not a permutation of [1,2,3] because it is the same" $
            isPermutation [1 :: Integer, 2, 3] [1, 2, 3] `shouldBe` (False :: Bool)

        it "[2,2,0] is not a permutation of [1,2,3]" $
            isPermutation [1 :: Integer, 2, 3] [2, 2, 0] `shouldBe` (False :: Bool)

        it "[1,2] is not a permutation of [1,2,3] because it is a smaller array" $
            isPermutation [1 :: Integer, 2] [1, 2, 3] `shouldBe` (False :: Bool)

        it "[1,2,3,4] is not a permutation of [1,2,3] because it is a bigger array" $
            isPermutation [1 :: Integer, 2, 3, 4] [1, 2, 3] `shouldBe` (False :: Bool)

    describe "Exercise 3: Recognizing and generating derangements" $ do
        it "[2,3,1] is a derangement of [1,2,3]" $
            isDerangement [1 :: Integer, 2, 3] [2, 3, 1] `shouldBe` (True :: Bool)

        it "[3,2,1] is not a derangement of [1,2,3] because 2 is in the 2nd position" $
            isDerangement [1 :: Integer, 2, 3] [3, 2, 1] `shouldBe` (False :: Bool)

        it "[2,3,4,1] is not a derangement of [1,2,3] because it has a different size" $
            isDerangement [1 :: Integer, 2, 3] [2, 3, 4, 1] `shouldBe` (False :: Bool)

        it "[2,3,4,1] is a derangement of [1,2,3,4]" $
            isDerangement [1 :: Integer, 2, 3, 4] [2, 3, 4, 1] `shouldBe` (True :: Bool)

        it "The function deran procudes derangements" $
            property  (\x -> all (isDerangement [0..x-1]) (deran x))

    describe "Exercise 4: " $ do
        it "NL39 RABO 0300 0652 64 is a valid Dutch IBAN" $
            iban "NL39 RABO 0300 0652 64" `shouldBe` (True :: Bool)

        it "ES80 2310 0001 1800 0001 2345 is a valid Spanish IBAN" $
            iban "ES80 2310 0001 1800 0001 2345" `shouldBe` (True :: Bool)

        it "DE89 3704 0044 0532 0130 00  is a valid German IBAN" $
            iban "DE89 3704 0044 0532 0130 00" `shouldBe` (True :: Bool)

        it "NL39 RABO 0300 065! 264$ is still a valid Dutch IBAN because the specs told us to filter ASCII characters" $
            iban "NL39 RABO 0300 065! 264$" `shouldBe` (True :: Bool)

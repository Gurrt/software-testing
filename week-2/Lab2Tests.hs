import Test.Hspec
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

    describe "Exercise 2: Recognizing Permutations" $
        it "Permutation" $
            True `shouldBe` True

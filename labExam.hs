import Data.List
import Test.QuickCheck

data Btree a = Leaf | B a (Btree a) (Btree a) deriving (Eq,Show)

-- Exercise 1 --
mapT :: ( a -> b) -> Btree a -> Btree b
mapT f Leaf = Leaf
mapT f (B x left right) = B (f x) (mapT f left) (mapT f right)

{-
We can prove this function is right by induction, first for the base case where the Tree consists out of a leaf the function
always works, since there is no data on those nodes. When there is an actual tree, we assume the function to work on the subtrees.
Which justs leaves us with the task of applying f to the data of the current node, and then calling the function on the subtrees, which is
exactly what the function is doing.
-}

-- Exercise 2 --

inOrder :: Btree a -> [a]
inOrder Leaf = []
inOrder (B x left right) = (inOrder left) ++ [x] ++ (inOrder right)

inOrderRev :: Btree a -> [a]
inOrderRev Leaf = []
inOrderRev (B x left right) = (inOrderRev right) ++ [x] ++ (inOrderRev left)

treeProperty :: Eq a => Btree a -> Bool
treeProperty t = inOrder t == reverse (inOrderRev t)

{-
A simple test with one or two trees should suffice, this property holds for all trees, so random testing is
overkill for this kind of property. 

-- TEST 1--
*Main> treeProperty (B 0 (B 2 Leaf Leaf) Leaf)
True
*Main> inOrder (B 0 (B 2 Leaf Leaf) Leaf)
[2,0]
*Main> inOrderRev (B 0 (B 2 Leaf Leaf) Leaf)
[0,2]

-- TEST 2--
*Main> treeProperty (B 0 (B 2 (B 32 Leaf Leaf) Leaf) (B 87 Leaf (B 34 Leaf Leaf) ))
True
*Main> inOrder (B 0 (B 2 (B 32 Leaf Leaf) Leaf) (B 87 Leaf (B 34 Leaf Leaf) ))
[32,2,0,87,34]
*Main> inOrderRev (B 0 (B 2 (B 32 Leaf Leaf) Leaf) (B 87 Leaf (B 34 Leaf Leaf) ))
[34,87,0,2,32]

Shows that the property works, and by design of the binary trees should hould for everything.
-}

-- Exercise 3 --

makeT :: Ord a => [a] -> Btree a
makeT = foldr insertT Leaf

insertT :: Ord a => a -> Btree a -> Btree a
insertT x Leaf = B x Leaf Leaf
insertT x (B y left right)
    | x < y = B y (insertT x left) right
    | otherwise = B y left (insertT x right)

sortList :: Ord a => [a] -> [a]
sortList xs = inOrder (makeT xs)

-- From : http://stackoverflow.com/questions/22050710/test-if-a-list-is-sorted
isSorted :: (Ord a) => [a] -> Bool
isSorted []       = True
isSorted [x]      = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

prop_sortList_sorts xs = isSorted $ sortList xs
    where types = xs :: [Int]
    
{-
*Main> quickCheck prop_sortList_sorts
+++ OK, passed 100 tests.
-}

-- Exercise 4 --
type Dict = Btree (String,String)

lemma, info :: (String,String) -> String
lemma (x,_) = x
info (_,y) = y

lookUp :: String -> Dict -> [String]
lookUp _ Leaf = []
lookUp word (B kv left right)
    | word < (lemma kv) = lookUp word left
    | word == (lemma kv) = [info (kv)]
    | otherwise = lookUp word right

{-
 In the base case, a dictionairy just consisting out of a leaf, the algorithm will return nothing.
 For the induction step we will assume that the algorithm works for the subtrees of the current tree and that the Dictionairy is indeed ordered with no duplicates.
 In that case we just need to compare the string to the lemma of the current node, if it is equal we found the word and we can return the info
 otherwise, if it is alphabetically lower we need to look in the left subtree, if it is higher in the right subtree. This is exactly what the algorithm is doing.
-}

-- Exercise 5 --

insertLemma :: (String,String) -> Dict -> Dict
insertLemma kv Leaf = B ((lemma kv), (info kv)) Leaf Leaf
insertLemma kv (B kv2 left right)
    | lemma kv == lemma kv2 = error "Lemma already exists in dictionairy"    
    | lemma kv < lemma kv2 = B kv2 (insertLemma kv left) right
    | lemma kv > lemma kv2 = B kv2 left (insertLemma kv right)

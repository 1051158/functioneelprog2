module Main where

import Data.Char
import Data.List
import System.Environment

-- | Definition of a binary tree (Bintree).
-- The tree can be either Empty or a Branch with a value and two subtrees.
data Bintree a = Empty
               | Branch a (Bintree a) (Bintree a)
               deriving (Show, Read)

-- | 'preorder' function performs a preorder traversal of the binary tree.
-- It visits the root node first, then the left subtree, and finally the right subtree.
preorder :: (Bintree a) -> [a]
preorder Empty = []
preorder (Branch a b c) = [a] ++ preorder b ++ preorder c

-- | 'postorder' function performs a postorder traversal of the binary tree.
-- It visits the left subtree first, then the right subtree, and finally the root node.
postorder :: (Bintree a) -> [a]
postorder Empty = []
postorder (Branch a b c) = postorder b ++ postorder c ++ [a]

-- | 'inorder' function performs an inorder traversal of the binary tree.
-- It visits the left subtree first, then the root node, and finally the right subtree.
inorder :: (Bintree a) -> [a]
inorder Empty = []
inorder (Branch a b c) = inorder b ++ [a] ++ inorder c

-- | 'push' function inserts an element into the binary tree, maintaining the order.
-- If the tree is empty, it creates a new Branch.
-- If the element is less than the root, it inserts it into the left subtree.
-- Otherwise, it inserts it into the right subtree.
push :: (Ord a) => (Bintree a) -> a -> (Bintree a)
push Empty a = Branch a Empty Empty
push (Branch a b c) d
  | d < a = Branch a (push b d) c
  | otherwise = Branch a b (push c d)

-- | 'pushlist' function inserts a list of elements into the binary tree.
-- It uses 'foldl' to apply 'push' to each element in the list.
pushlist :: (Ord a) => (Bintree a) -> [a] -> (Bintree a)
pushlist = foldl push

-- | 'maptree' function applies a function to each element of the binary tree.
-- It returns a new tree with the function applied to each element.
maptree :: (a -> b) -> (Bintree a) -> (Bintree b)
maptree function Empty = Empty
maptree function (Branch a b c) = Branch (function a) (maptree function b) (maptree function c)

-- | 'filtertree' function filters elements of the binary tree based on a predicate.
-- It returns a list of elements that satisfy the predicate.
filtertree :: (a -> Bool) -> (Bintree a) -> [a]
filtertree bool Empty = []
filtertree bool (Branch a b c) = [a | bool a] ++ filtertree bool b ++ filtertree bool c

main = do
  -- | Get command-line arguments for the source file name.
  [sourcefile] <- getArgs

  -- | Read the content of the source file.
  filecontent <- readFile sourcefile
  
  -- | Filter out all whitespace characters.
  let filteredContent = filter (not . isSpace) filecontent
  
  -- | Insert the filtered file content into the binary tree.
  let tree = pushlist Empty filteredContent

  -- | Convert the tree elements to their integer ASCII values.
  let intTree = maptree ord tree

  -- | Write the integer tree to a file.
  writeFile "tree.txt" (show intTree)

  -- | Read the integer tree from the file.
  strTree2 <- readFile "tree.txt"
  let tree2b = (read strTree2 :: Bintree Int)

  -- | Convert the integer tree back to characters.
  let tree3 = maptree chr tree2b

  -- | Perform an inorder traversal of the character tree and print the result.
  let string = inorder tree3
  putStrLn string

  -- | Filter the tree to include only digit characters and print the result.
  let filteredTree = filtertree isDigit tree3
  putStrLn filteredTree

  putStrLn "done"

-- Explanation:
-- This Haskell program reads a string from a file, filters out whitespace characters,
-- processes the filtered string into a binary tree, performs various operations on the tree,
-- and writes/reads the tree to/from a file. Finally, it prints the inorder traversal of the tree
-- and the filtered tree containing only digits.

-- Data Structure:
-- - 'Bintree' is a binary tree that can be empty or a branch with a value and two subtrees.

-- Tree Traversal Functions:
-- - 'preorder', 'postorder', and 'inorder' perform different types of tree traversals.

-- Tree Manipulation Functions:
-- - 'push' inserts an element into the binary tree.
-- - 'pushlist' inserts a list of elements into the binary tree.
-- - 'maptree' applies a function to each element in the binary tree.
-- - 'filtertree' filters elements in the binary tree based on a predicate.

-- Main Function:
-- 1. Get the source file name from command-line arguments.
-- 2. Read the content of the source file.
-- 3. Filter out whitespace characters.
-- 4. Insert the filtered file content into the binary tree.
-- 5. Convert the tree elements to their integer ASCII values.
-- 6. Write the integer tree to a file.
-- 7. Read the integer tree from the file.
-- 8. Convert the integer tree back to characters.
-- 9. Perform an inorder traversal of the character tree and print the result.
-- 10. Filter the tree to include only digit characters and print the result.
-- 11. Print "done" to indicate completion.

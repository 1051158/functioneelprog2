module Main where

import System.Environment

-- | Definition of a Huffman coding tree (Codetree).
-- The tree can be either a Branchy node with an integer value and two subtrees,
-- or a Branchy2 leaf node with an integer value and a character.
data Codetree a = Branchy Int (Codetree a) (Codetree a)
                | Branchy2 Int Char
                deriving (Show, Eq, Ord, Read)

-- | 'split' function decodes a single character from the bit string using the Huffman tree.
-- It traverses the tree based on the bits ('1' for left, '0' for right) until a leaf node is reached.
split :: Codetree a -> [Char] -> (Char, [Char])
split (Branchy2 _ chr) bits = (chr, bits)
split (Branchy _ tree1 tree2) bits
  | head bits == '1' = split tree1 rest
  | otherwise = split tree2 rest
  where rest = drop 1 bits

-- | 'decompress' function decodes the entire bit string using the Huffman tree.
-- It repeatedly applies 'split' to decode each character.
decompress :: Codetree a -> [Char] -> [Char]
decompress _ [] = []
decompress tree bitString = fst tuple : decompress tree (snd tuple)
  where tuple = split tree bitString

main = do
  -- | Get command-line arguments for the source file, target file, and codetree file names.
  [sourcefile, targetfile, codetreefile] <- getArgs

  -- | Read the content of the source file (bit string to be decompressed).
  filecontent <- readFile sourcefile

  -- | Read the Huffman coding tree from the codetree file.
  strCodetree <- readFile codetreefile
  let codetree = (read strCodetree :: Codetree a)

  -- | Decompress the file content using the Huffman coding tree.
  let uncompressedContent = decompress codetree filecontent

  -- | Calculate the length of the decompressed content.
  let lenUncompressed = length uncompressedContent

  -- | Print the length of the decompressed content in characters and bits.
  putStrLn $ "length of decompressed file: " ++ show lenUncompressed ++ " characters, " ++ show (lenUncompressed  * 8) ++ " bits."

  -- | Write the decompressed content to the target file.
  writeFile targetfile uncompressedContent
  putStrLn $ targetfile ++ " written to disk..."

  putStrLn "done"

-- Explanation:
-- This Haskell program decompresses a bit string using a Huffman coding tree.
-- It reads the bit string from a source file and the Huffman coding tree from a codetree file,
-- decodes the bit string using the tree, and writes the decompressed content to a target file.

-- Huffman Coding Tree:
-- - 'Codetree' is a binary tree that can be either a Branchy node with two subtrees or a Branchy2 leaf node with a character.
-- - 'split' traverses the tree based on bits to decode a single character.
-- - 'decompress' decodes the entire bit string by repeatedly applying 'split'.

-- Main Function:
-- 1. Get the source file, target file, and codetree file names from command-line arguments.
-- 2. Read the bit string to be decompressed from the source file.
-- 3. Read the Huffman coding tree from the codetree file.
-- 4. Decompress the bit string using the Huffman coding tree.
-- 5. Calculate and print the length of the decompressed content in characters and bits.
-- 6. Write the decompressed content to the target file.
-- 7. Print "done" to indicate completion.

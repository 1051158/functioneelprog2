module Main where

import Data.List
import System.Environment
import Data.Functor
import Control.Applicative

-- | Step 1 of Huffman encoding: Count the frequency of each character in the string.
-- The result is a list of tuples (frequency, character), sorted in descending order of frequency.
huffmanStap1 :: String -> [(Int, Char)]
huffmanStap1 str = reverse (sortOn fst (map (\str -> (length str, head str)) (group (sort str))))

-- | Definition of a Huffman coding tree.
data Codetree a = Branchy Int (Codetree a) (Codetree a)
                | Branchy2 Int Char
                deriving (Show, Eq, Ord, Read)

-- | 'value' function retrieves the frequency value from a Huffman coding tree node.
value :: Codetree a -> Int
value (Branchy a _ _) = a
value (Branchy2 a _) = a 

-- | Prepare the initial list of Huffman coding tree nodes from the frequency table.
prepHuffmanStap2 :: [(Int, Char)] -> [Codetree a]
prepHuffmanStap2 tuples = sort [Branchy2 int chr | (int, chr) <- tuples]

-- | Step 2 of Huffman encoding: Build the Huffman coding tree.
huffmanStep2 :: [Codetree a] -> [Codetree a]
huffmanStep2 (x:xs:xss) = huffmanStep2 (sort (Branchy (value x + value xs) x xs : xss))
huffmanStep2 x = x

-- | Step 3 of Huffman encoding: Generate the encoding table from the Huffman coding tree.
huffmanStap3 :: Codetree a -> [Char] -> [([Char], Char)]
huffmanStap3 (Branchy2 int chr) bits = [(bits, chr)]
huffmanStap3 (Branchy int tree1 tree2) bits = huffmanStap3 tree1 (bits++"1") ++ huffmanStap3 tree2 (bits++"0")

-- | Convert a character to its corresponding bits using the encoding table.
chrToBits :: Char -> [([Char], Char)] -> [Char]
chrToBits chr table = fst (head (filter condition table))
  where condition (_, tableChr) = tableChr == chr

-- | Step 4 of Huffman encoding: Encode the entire string using the encoding table.
huffmanStap4 :: [([Char], Char)] -> [Char] -> [Char]
huffmanStap4 table str = concat [chrToBits chr table | chr <- str]

main = do
  -- | Get command-line arguments for source file, target file, and codetree file names.
  [sourcefile, targetfile, codetreefile] <- getArgs
  
  -- | Read the content of the source file.
  filecontent <- readFile sourcefile

  -- | Build the Huffman coding tree from the file content.
  let codetree = head (huffmanStep2 (prepHuffmanStap2 (huffmanStap1 filecontent)))
  
  -- | Compress the file content using the Huffman coding tree.
  let compressedContent = huffmanStap4 (sortOn fst (huffmanStap3 codetree [])) filecontent

  -- | Calculate the length of the source content in bits and the length of the compressed content.
  let lenSource = length filecontent * 8
  let lenCompressed = length compressedContent
  let factor = round (fromIntegral lenCompressed / fromIntegral lenSource * 100)

  -- | Print the length of the source and compressed content in bits, and the compression factor.
  putStrLn $ "length of " ++ sourcefile ++ ": " ++ show (lenSource `div` 8) ++ " characters, " ++ show lenSource ++ " bits."
  putStrLn $ "length of compressed file " ++ targetfile ++ ": " ++ show lenCompressed ++ " bits."
  putStrLn $ "factor " ++ show lenCompressed ++ "/" ++ show lenSource ++ "*100=" ++ show factor ++ "%"

  -- | Write the compressed content to the target file.
  writeFile targetfile compressedContent
  putStrLn $ targetfile ++ " written to disk..."

  -- | Write the Huffman coding tree to the codetree file.
  writeFile codetreefile (show codetree)
  putStrLn $ show codetree ++ " written to disk..."

  putStrLn "done"

-- Explanation:
-- This Haskell program performs Huffman encoding on the contents of a file.
-- It reads the file specified by the first command-line argument, compresses its contents using Huffman encoding,
-- and writes the compressed content and the Huffman coding tree to the specified output files.
-- Additionally, it prints the length of the original and compressed contents in bits, and the compression factor.

-- Huffman Encoding Steps:
-- 1. 'huffmanStap1': Count the frequency of each character in the string and create a sorted frequency table.
-- 2. 'prepHuffmanStap2': Prepare the initial list of Huffman coding tree nodes from the frequency table.
-- 3. 'huffmanStep2': Build the Huffman coding tree by combining nodes with the smallest frequencies.
-- 4. 'huffmanStap3': Generate the encoding table from the Huffman coding tree.
-- 5. 'huffmanStap4': Encode the entire string using the encoding table.

-- Helper Functions:
-- - 'value': Retrieves the frequency value from a Huffman coding tree node.
-- - 'chrToBits': Converts a character to its corresponding bits using the encoding table.

-- Main Function:
-- 1. Get the source file, target file, and codetree file names from command-line arguments.
-- 2. Read the content of the source file.
-- 3. Build the Huffman coding tree from the file content.
-- 4. Compress the file content using the Huffman coding tree.
-- 5. Calculate the length of the source and compressed content in bits, and the compression factor.
-- 6. Print the length of the source and compressed content in bits, and the compression factor.
-- 7. Write the compressed content to the target file.
-- 8. Write the Huffman coding tree to the codetree file.
-- 9. Print "done" to indicate completion.

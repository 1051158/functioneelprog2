module Main where

import Data.List
import Data.Char
import System.Environment
import Text.Read

-- | 'decompress' function takes a compressed string and returns the decompressed string.
-- If the input string is empty, it returns an empty list.
-- If the first character is not a digit, it takes that character as is and proceeds.
-- Otherwise, it reads the digits as the number of times to replicate the following character.
decompress :: [Char] -> [Char]
decompress x
  | null x = []
  | index == 0 = take 1 x ++ rest
  | otherwise = replicate (read (take index x) :: Int) (x !! index) ++ rest
  where
    -- | 'index' is the number of initial digits in the string.
    index = length (takeWhile isDigit x)
    -- | 'rest' is the remaining string after processing the current segment.
    rest = decompress (drop (index+1) x)

main = do
  -- | Get command-line arguments for source and target file names.
  [sourcefile, targetfile] <- getArgs

  -- | Read the content of the source file.
  filecontent <- readFile sourcefile

  -- | Decompress the content using the 'decompress' function.
  let uncompressedContent = decompress filecontent

  -- | Write the decompressed content to the target file.
  writeFile targetfile uncompressedContent
  putStrLn "done..."

-- Explanation:
-- This Haskell program performs decompression of a run-length encoded string from a file.
-- It reads the file specified by the first command-line argument, decompresses its contents,
-- and writes the decompressed content to the file specified by the second command-line argument.

-- Decompression:
-- - The 'decompress' function interprets sequences where a digit indicates the number of times
--   the subsequent character is repeated.
-- - For example, "3a2b1c" -> "aaabbc".

-- Function 'decompress':
-- - Type signature: 'decompress :: [Char] -> [Char]'
-- - Takes a compressed string and returns the decompressed string.
-- - Uses 'takeWhile isDigit' to count the digits at the beginning of the string.
-- - 'replicate' is used to repeat the character based on the read number.

-- Pattern Matching and Guards:
-- - If the input string is empty, it returns an empty list.
-- - If there are no leading digits, it takes the first character as is.
-- - Otherwise, it reads the digits, replicates the following character, and processes the rest of the string.

-- File I/O:
-- - 'getArgs' retrieves command-line arguments.
-- - 'readFile' reads the content of a file.
-- - 'writeFile' writes content to a file.
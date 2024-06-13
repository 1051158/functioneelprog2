module Main where

import Data.List
import System.Environment

-- | 'check' function takes a string and returns a compressed string.
-- If the input string has a length of 1, it returns the string as is.
-- Otherwise, it returns the length of the string followed by its first character.
check :: String -> String
check x
    | length x == 1 = x
    | otherwise = show (length x) ++ [head x]

-- | 'compress' function takes a string and applies run-length encoding to it.
-- It groups consecutive identical elements and applies 'check' to each group,
-- then concatenates the results.
compress :: String -> String
compress input = concatMap check (group input)

main = do 
    -- | Get command-line arguments for source and target file names.
    [sourcefile, targetfile] <- getArgs
    
    -- | Read the content of the source file.
    filecontent <- readFile sourcefile

    -- | Compress the content using the 'compress' function.
    let compressedContent = compress filecontent
    let lenSource = length filecontent
    let lenCompressed = length compressedContent
    
    -- | Calculate the compression factor as a percentage.
    let factor = round (fromIntegral lenCompressed / fromIntegral lenSource * 100)

    -- | Print statistics about the compression.
    putStrLn $ "length of " ++ sourcefile ++ ": " ++ show lenSource ++ " characters"
    putStrLn $ "length of compressed file " ++ targetfile ++ ": " ++ show lenCompressed ++ " characters"
    putStrLn $ "factor " ++ show lenCompressed ++ "/" ++ show lenSource ++ "*100=" ++ show factor ++ "%"

    -- | Write the compressed content to the target file.
    writeFile targetfile compressedContent
    putStrLn "done..."

-- Explanation:
-- This Haskell program performs run-length encoding (RLE) on the contents of a file.
-- It reads the file specified by the first command-line argument, compresses its contents,
-- and writes the compressed content to the file specified by the second command-line argument.
-- The program also prints the length of the original and compressed contents, as well as the compression factor.

-- Run-length Encoding (RLE):
-- - RLE is a simple compression technique that replaces sequences of the same character
--   with the character followed by the number of occurrences.
-- - For example, "aaabbc" -> "3a2b1c".

-- Function 'check':
-- - Type signature: 'check :: String -> String'
-- - Takes a string and returns a compressed string.
-- - If the input string has a length of 1, it returns the string as is.
-- - Otherwise, it returns the length of the string followed by its first character.

-- Function 'compress':
-- - Type signature: 'compress :: String -> String'
-- - Takes a string and applies run-length encoding to it.
-- - Uses 'group' from Data.List to group consecutive identical elements.
-- - Uses 'concatMap' to apply 'check' to each group and concatenate the results.

-- File I/O:
-- - 'getArgs' retrieves command-line arguments.
-- - 'readFile' reads the content of a file.
-- - 'writeFile' writes content to a file.

-- Statistics Calculation:
-- - 'lenSource' is the length of the original content.
-- - 'lenCompressed' is the length of the compressed content.
-- - 'factor' is the compression ratio as a percentage, calculated as:
--   (length of compressed content / length of original content) * 100

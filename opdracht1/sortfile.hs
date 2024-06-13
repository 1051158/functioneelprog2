module Main where

import Data.List
import System.Environment

main = do 
    -- | Get command-line arguments for source and target file names.
    [sourcefile, targetfile] <- getArgs
    
    -- | Read the content of the source file.
    filecontent <- readFile sourcefile
    print filecontent  -- | Print the original file content.
    
    -- | Sort the file content using the 'sort' function from Data.List.
    let sortedFilecontent = sort filecontent
    print sortedFilecontent  -- | Print the sorted file content.
    
    -- | Write the sorted content to the target file.
    writeFile targetfile sortedFilecontent

-- Explanation:
-- This Haskell program reads the content of a file, sorts the content, and writes the sorted content to a new file.
-- It also prints the original and sorted content to the console for verification.

-- Function 'sort':
-- - The 'sort' function from Data.List is used to sort the characters in the file content.
-- - Sorting is done in ascending order (lexicographical order for strings).

-- File I/O:
-- - 'getArgs' retrieves command-line arguments for the source and target file names.
-- - 'readFile' reads the content of the source file.
-- - 'writeFile' writes the sorted content to the target file.

-- Steps in Main:
-- 1. Get the source and target file names from the command-line arguments.
-- 2. Read the content of the source file.
-- 3. Print the original file content.
-- 4. Sort the file content.
-- 5. Print the sorted file content.
-- 6. Write the sorted content to the target file.

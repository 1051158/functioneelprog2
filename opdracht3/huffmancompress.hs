module Main where

import Data.List
import System.Environment

import Data.Functor

import Control.Applicative

example :: String
example = "aaabbcbbcccddccbbcccdecccaaaaabbbb"

huffmanStap1 :: String -> [(Int, Char)]
huffmanStap1 str = reverse (sortOn fst (map (\str -> (length str, head str) ) (group (sort str))))

data Codetree a = Branchy Int (Codetree a) (Codetree a)
                | Branchy2 Int Char
                deriving (Show, Eq, Ord, Read)

value :: Codetree a -> Int
value (Branchy a _ _) = a
value (Branchy2 a _) = a 

prepHuffmanStap2 :: [(Int, Char)] -> [Codetree a]
prepHuffmanStap2 tuples = sort [Branchy2 int chr | (int, chr) <- tuples]

huffmanStep2 :: [Codetree a] -> [Codetree a]
huffmanStep2 (x:xs:xss) = huffmanStep2 (sort (Branchy (value x + value xs) x xs : xss))
huffmanStep2 x = x


huffmanStap3 :: Codetree a -> [Char] -> [([Char], Char)]
huffmanStap3 (Branchy2 int chr) bits = [(bits, chr)]
huffmanStap3 (Branchy int tree1 tree2) bits = huffmanStap3 tree1 (bits++"1") ++ huffmanStap3 tree2 (bits++"0")


chrToBits :: Char -> [([Char], Char)] -> [Char]
chrToBits chr table = fst (head (filter condition table))
  where condition (_, tableChr) = tableChr == chr

huffmanStap4 :: [([Char], Char)] -> [Char] -> [Char]
huffmanStap4 table str = concat [chrToBits chr table | chr <- str]


main = do [sourcefile, targetfile, codetreefile] <- getArgs
          filecontent <- readFile sourcefile

          let codetree = head (huffmanStep2 (prepHuffmanStap2 (huffmanStap1 filecontent)))
          let compressedContent = huffmanStap4 (sortOn fst (huffmanStap3 codetree [])) filecontent

          let lenSource = length filecontent * 8
          let lenCompressed = length compressedContent
          let factor = round (fromIntegral lenCompressed / fromIntegral lenSource * 100)

          putStrLn $ "length of " ++ sourcefile ++ ": " ++ show (lenSource `div` 8) ++ " characters, " ++ show lenSource ++ " bits."
          putStrLn $ "length of compressed file " ++ targetfile ++ ": " ++ show lenCompressed ++ " bits."

          putStrLn $ "factor " ++ show lenCompressed ++ "/" ++ show lenSource ++ "*100=" ++ show factor ++ "%"

          writeFile targetfile compressedContent
          putStrLn $ targetfile ++ " written to disk..."

          writeFile codetreefile (show codetree)
          putStrLn $ show codetree ++ "written to disk..."

          putStrLn "done"

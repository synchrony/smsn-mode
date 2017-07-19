-- This expects the input file to consist entirely of foldable org lines.
  -- That is, each line starts with at least one *, followed by a space.
-- If a line starts with n asterisks, then
  -- the first n-1 asterisks are each converted to four spaces
  -- and the last one is left intact, along with the rest of the line.
-- Any other kind of line will be retained without change.

-- Run it using 'runghc' (part of a standard Haskell installation).
  -- runghc org-to-smsn-mode.hs file.org file.smsn

import Data.List (span)
import System.Environment (getArgs)

data IndentLine = IndentLine Int String

parseFromOrg :: String -> IndentLine
parseFromOrg s = IndentLine level $ tail rest -- tail strips a leading ' '
  where (prefix,rest) = span (== '*') s
        level = length prefix

serializeToSmsn :: IndentLine -> String
serializeToSmsn (IndentLine 0 s) = error
  $ "Level-0 string encountered."
  ++ " (Those are valid in org-mode but not smsn-mode.)"
serializeToSmsn (IndentLine k s) = replicate (4 * k - 4) ' ' ++ "* " ++ s

f = serializeToSmsn . parseFromOrg

-- a faster yet worse alternative
  -- less safe, cryptic errors, hard to generalize
f' :: String -> String
f' s = g 0 s where
  g :: Int -> String -> String
  g k ('*':'*':rest) = g (k+1) ('*':rest)
  g k ('*':rest) = (concat $ replicate (4*k) " ") ++ "* " ++ rest
  g _ s = s

main :: IO ()
main = do
  [inputFile, outputFile] <- getArgs
  input <- readFile inputFile
  writeFile outputFile $ unlines $ map f $ lines input

-- a pipe-friendly alternative. takes from stdin, puts to stdout.
  -- example: cat in.org | runghc org-to-smsn-mode.hs > out.smsn
main' = interact $ unlines . map f . lines

-- This expects the input file to consist entirely of foldable org lines.
  -- That is, each line starts with at least one *, followed by a space.
-- If a line starts with n asterisks, then
  -- the first n-1 asterisks are each converted to four spaces
  -- and the last one is left intact, along with the rest of the line.
-- Any other kind of line will be retained without change.

-- Run it using 'runghc' (part of a standard Haskell installation).
  -- runghc org-to-smsn-mode.hs file.org file.smsn

import System.Environment (getArgs)

f :: String -> String
f s = g 0 s where
  g :: Int -> String -> String
  g k ('*':'*':rest) = g (k+1) ('*':rest)
  g k ('*':rest) = (concat $ replicate (4*k) " ") ++ "* " ++ rest
  g _ s = s

main :: IO ()
main = do
  [inputFile, outputFile] <- getArgs
  input <- readFile inputFile
  writeFile outputFile $ concat $ map ((\s -> s ++ "\n") . f) $ lines input

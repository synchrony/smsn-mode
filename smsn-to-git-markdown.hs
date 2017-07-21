{-# LANGUAGE ViewPatterns #-}
-- each line of ordinary text needs to be preceded by a newline
-- but each line of a codeblock, including the bracketing ``` lines, don't.

import Data.List (span, stripPrefix)
import Data.List.Split (splitWhen)
import Data.Maybe
import System.Environment (getArgs)


stripLeadingSpace :: String -> String
stripLeadingSpace = snd . span (== ' ')

-- assumes input looks like "* :OETCJmx4rJmIR5Pk: bla bla", keeps only blas
stripSmsnAddress :: String -> String
stripSmsnAddress = drop 21

readSmsnLines :: String -> [ExportedSmsnLine]
readSmsnLines =
  map (exportedSmsnLine . stripSmsnAddress . stripLeadingSpace)
  . lines


data ExportedSmsnLine = File String | Text String | Code String | Ignore
  deriving Show

exportedSmsnLine :: String -> ExportedSmsnLine
exportedSmsnLine (stripPrefix "[markdown]" -> Just restOfLine)
  = Text restOfLine -- these need to be prefixed with '\n'
exportedSmsnLine (stripPrefix "[markdown-code]" -> Just restOfLine)
  = Code restOfLine
exportedSmsnLine (stripPrefix "[target-filename]" -> Just restOfLine)
  = File restOfLine
exportedSmsnLine s = Ignore

markdown :: [ExportedSmsnLine] -> String
markdown es = unlines $ mapMaybe f es where
  f :: ExportedSmsnLine -> Maybe String
  f (File s) = Nothing
  f (Text s) = Just $ "\n" ++ s
  f (Code s) = Just s
  f Ignore = Nothing

pairFilesToContents :: [ExportedSmsnLine] -> [(FilePath, String)]
pairFilesToContents stuff = zip files contents where
  files = map (\(File s) -> s) $ filter isFile stuff
  contents = map markdown $ tail $ splitWhen isFile stuff
    -- use tail to ignore anything before the first filename

isFile (File _) = True
isFile _ = False

main = do
  (inputFile:_) <- getArgs
  --let inputFile = "input.auto.md"
  input <- readFile inputFile
  let pairs = pairFilesToContents $ readSmsnLines input
  mapM_ f pairs where
    f :: (FilePath, String) -> IO ()
    f (name, content) = writeFile name content

-- ======= the old way
-- =======
main' = interact f
 -- example of how to run it
   -- cat input.auto.md | runghc smsn-to-git-markdown.hs

markdownLine :: String -> Maybe String
markdownLine = stripPrefix "[markdown"

processMarkdownLine :: String -> String
  -- newlines make the unrendered markdown more readable
    -- except in a code block, where they would hinder readability
processMarkdownLine (stripPrefix "]"           -> Just restOfLine) =
  '\n' : restOfLine
processMarkdownLine (stripPrefix "-code]"      -> Just restOfLine) =
  restOfLine
processMarkdownLine s = "ERROR: ANOTHER CODE? " ++ s

f :: String -> String
f theFile = unlines $ map processMarkdownLine $ catMaybes
  $ map (markdownLine . stripSmsnAddress . stripLeadingSpace)
  $ lines theFile

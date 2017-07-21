{-# LANGUAGE ViewPatterns #-}
-- each line of ordinary text needs to be preceded by a newline
-- but each line of a codeblock, including the bracketing ``` lines, don't.

import Data.List (span, stripPrefix)
import Data.List.Split (splitWhen)
import Data.Maybe
import System.Process

stripLeadingSpace :: String -> String
stripLeadingSpace = snd . span (== ' ')

-- assumes input looks like "* :OETCJmx4rJmIR5Pk: bla bla", keeps only blas
stripSmsnAddress :: String -> String
stripSmsnAddress = drop 21

readSmsnLines :: String -> [ExportedSmsnLine]
readSmsnLines = map (exportedSmsnLine . stripSmsnAddress . stripLeadingSpace)
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

assignFiles :: [ExportedSmsnLine] -> [(FilePath, [ExportedSmsnLine])]
assignFiles stuff = zip files contents where
  files = map (\(File s) -> s) $ filter isFile stuff
  contents = tail $ splitWhen isFile stuff

isFile (File _) = True
isFile _ = False


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

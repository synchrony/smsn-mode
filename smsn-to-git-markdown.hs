{-# LANGUAGE ViewPatterns #-}
-- each line of ordinary text needs to be preceded by a newline
-- but each line of a codeblock, including the bracketing ``` lines, don't.

import Data.List (span, stripPrefix)
import Data.Maybe
import System.Process

stripLeadingSpace :: String -> String
stripLeadingSpace = snd . span (== ' ')

-- assumes input looks like "* :OETCJmx4rJmIR5Pk: bla bla", keeps only blas
stripSmsnAddress :: String -> String
stripSmsnAddress = drop 21

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

data ExportedSmsn = File String | Text String | Code String | Ignore
  deriving Show
exportedSmsn (stripPrefix "[markdown]" -> Just restOfLine)
  = Text $ '\n' : restOfLine
exportedSmsn (stripPrefix "[markdown-code]" -> Just restOfLine)
  = Code restOfLine
exportedSmsn (stripPrefix "[target-filename]" -> Just restOfLine)
  = File restOfLine
exportedSmsn s = Ignore

-- file <- readFile "input.auto.md" 
-- let repd = map (exportedSmsn . stripSmsnAddress . stripLeadingSpace) $ lines file
-- map (\x -> case x of Code _ -> x; _ -> Ignore) repd

main = interact f
 -- example of how to run it
   -- cat input.auto.md | runghc smsn-to-git-markdown.hs

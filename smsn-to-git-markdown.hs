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
processMarkdownLine (stripPrefix "-code-start]" -> Just restOfLine) =
  '\n' : restOfLine
processMarkdownLine (stripPrefix "-code]"      -> Just restOfLine) =
  restOfLine
processMarkdownLine s = "ANOTHER CODE? " ++ s

f :: String -> String
f theFile = unlines $ map processMarkdownLine $ catMaybes
  $ map (markdownLine . stripSmsnAddress . stripLeadingSpace)
  $ lines theFile

main = interact f
 -- example of how to run it
   -- cat input.auto.md | runghc smsn-to-git-markdown.hs

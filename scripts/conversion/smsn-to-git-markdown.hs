-- run it from the command line:
  -- runghc smsn-to-git-markdown.hs INPUT_FILE

{-# LANGUAGE ViewPatterns #-}

import Data.List (span, stripPrefix)
import Data.List.Split (splitWhen)
import Data.Maybe
import System.Environment (getArgs)


readSmsnLines :: String -> [ExportedSmsnLine]
readSmsnLines =
  map (exportedSmsnLine . stripSmsnAddress . stripLeadingSpace)
  . lines

stripLeadingSpace :: String -> String
stripLeadingSpace = snd . span (== ' ')

-- assumes input looks like "* :OETCJmx4rJmIR5Pk: bla bla", keeps only blas
stripSmsnAddress :: String -> String
stripSmsnAddress = drop 21


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
  -- In markdown, all ordinary text lines need to be preceded by a newline,
  -- but lines of code, including the bracketing ``` lines, do not.

pairFilesToContents :: [ExportedSmsnLine] -> [(FilePath, String)]
pairFilesToContents stuff = zip files contents where
  files = map (\(File s) -> s) $ filter isFile stuff
  contents = map markdown $ tail $ splitWhen isFile stuff
    -- use tail to ignore anything before the first filename

isFile (File _) = True
isFile _ = False

main = do
  (inputFile:_) <- getArgs
  input <- readFile inputFile
  let pairs = pairFilesToContents $ readSmsnLines input
  writeFile "all makrdown, concatenated.txt" $ unlines $ map snd pairs
  mapM_ f pairs where
    f :: (FilePath, String) -> IO ()
    f (name, content) = writeFile name content

-- maybe better
main' = do
  (root : inputFile :_)  <- getArgs
  input <- readFile inputFile
  let pairs = pairFilesToContents $ readSmsnLines input
  mapM_ (f root) pairs where
    -- root is in the where clause's scope only if it is an argument to mapM_
    f :: FilePath -> (FilePath, String) -> IO ()
    f root (name, content) = writeFile (root ++ name) content

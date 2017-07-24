-- run it from the command line:
  -- runghc smsn-to-git-markdown.hs INPUT_FILE

{-# LANGUAGE ViewPatterns #-}

import Data.List (span, stripPrefix)
import Data.List.Split (splitWhen, splitOn)
import Data.Maybe
import System.Environment (getArgs)

type IndentLevel = Int
type UrlCount = Int

readSmsnLines :: String -> [Command]
readSmsnLines =
  map (readCommand . stripSmsnAddress . stripLeadingSpace)
  . lines

readSmsnLines' :: String -> [(IndentLevel, Command)]
readSmsnLines' s = map f levelsPairedWithAddressedStrings where
  levelsPairedWithAddressedStrings = map countLeadingSpace $ lines s
  f (lev,s) = (lev, readCommand $ stripSmsnAddress s)

stripLeadingSpace :: String -> String
stripLeadingSpace = snd . span (== ' ')

countLeadingSpace :: String -> (IndentLevel, String)
countLeadingSpace s = (n, s') where
  aSplit = span (== ' ') s
  n = length $ fst $ aSplit
  s' = snd $ aSplit

-- assumes input looks like "* :OETCJmx4rJmIR5Pk: bla bla", keeps only blas
stripSmsnAddress :: String -> String
stripSmsnAddress = drop 21

data Command = File String
             | Text String
             | Code String
             | Url String
             | Ignore         deriving Show

countUrlSubstitutions :: String -> UrlCount
countUrlSubstitutions s = f 0 s where
  f n (']' : '(' : ')' : s') = f (n+1) s'
  f n (_:s') = f n s'
  f n [] = n

interleave :: [String] -> [String] -> String
interleave outer inner = concat $ tail $ concat -- drops the "discard"
  $ map (\(a,b) -> [a,b])
  $ zip ("discard" : inner) outer -- inner is one shorter than outer

substituteUrlsOnce :: String -> [String] -> String
substituteUrlsOnce s urls =  interleave sDivided urlsBracketed
    where sDivided = splitOn "]()" s
          urlsBracketed = map (\url -> "](" ++ url ++ ")") urls

-- this should consume all URLs; if not, the input text has mismatches
substituteUrls :: [(IndentLevel,Command)] -> [Command]
substituteUrls levComs = f [] levComs where
  f :: [Command] -> [(IndentLevel,Command)] -> [Command]
  f accumulator ((lev, Text s) : rest) = let
    urlCount = countUrlSubstitutions s
    sWithSubs = substituteUrlsOnce s
               $ map (fromUrl . snd)
               $ take urlCount rest
    in f (Text sWithSubs : accumulator) $ drop urlCount rest
  f accumulator ((lev, com) : rest) = f (com : accumulator) rest
  f accumulator [] = accumulator

substituteUrls_test = substituteUrls
  [ (2, Text "[frog]() and [goat]() are friends")
  ,  (3, Url "http://frog")
  ,  (3, Url "http://goat")
  ]

fromUrl :: Command -> String
fromUrl (Url s) = s
fromUrl _ = error "fromUrl applied to non-URL"

readCommand :: String -> Command
readCommand (stripPrefix "[markdown]" -> Just restOfLine)
  = Text restOfLine -- these need to be prefixed with '\n'
readCommand (stripPrefix "[markdown-code]" -> Just restOfLine)
  = Code restOfLine
readCommand (stripPrefix "[target-filename]" -> Just restOfLine)
  = File restOfLine
readCommand (stripPrefix "[url-leaf]" -> Just restOfLine)
  = Url restOfLine
readCommand s = Ignore

markdown :: [Command] -> String
markdown es = unlines $ mapMaybe f es where
  f :: Command -> Maybe String
  f (File s) = Nothing
  f (Text s) = Just $ "\n" ++ s
  f (Code s) = Just s
  f (Url u) = Just $ error "Unusued Url: " ++ u
  f Ignore = Nothing
  -- In markdown, all ordinary text lines need to be preceded by a newline,
  -- but lines of code, including the bracketing ``` lines, do not.

pairFilesToContents :: [Command] -> [(FilePath, String)]
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

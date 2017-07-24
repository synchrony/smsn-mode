-- run it from the command line:
  -- runghc smsn-to-git-markdown.hs INPUT_FILE

-- TODO: Divide some steps
  -- data ExprGroup = AsIs [Expr] | Substitution [(IndentLevel,Expr)]
  -- then test each substitution for validity:
    -- first a Text, then Urls, and each Url a level lower than the Text

-- It might work already, but it's dangerous: errors can be cryptic, and in at least one case (not enough URLs) failure can look like success.

{-# LANGUAGE ViewPatterns #-}

import Data.List (span, stripPrefix)
import Data.List.Split (splitWhen, splitOn)
import Data.Maybe
import System.Environment (getArgs)

type IndentLevel = Int
type UrlCount = Int

data Expr = File String
          | Text String
          | Code String
          | Url String
          | Ignore      deriving Show

smsnLineToExpr :: String -> Expr
smsnLineToExpr (stripPrefix "[markdown]" -> Just restOfLine)
  = Text restOfLine -- these need to be prefixed with '\n'
smsnLineToExpr (stripPrefix "[markdown-code]" -> Just restOfLine)
  = Code restOfLine
smsnLineToExpr (stripPrefix "[target-filename]" -> Just restOfLine)
  = File restOfLine
smsnLineToExpr (stripPrefix "[url-leaf]" -> Just restOfLine)
  = Url restOfLine
smsnLineToExpr s = Ignore

buildMarkdownFile :: [Expr] -> String
buildMarkdownFile es = unlines $ mapMaybe f es where
  f :: Expr -> Maybe String
  f (File s) = Nothing
  f (Text s) = Just $ "\n" ++ s
  f (Code s) = Just s
  f (Url u) = Just $ error "Url mismatch: " ++ u
  f Ignore = Nothing
  -- In markdown, all ordinary text lines need to be preceded by a newline,
  -- but lines of code, including the bracketing ``` lines, do not.

isFile (File _) = True
isFile _ = False

fromUrl :: Expr -> String
fromUrl (Url s) = s
fromUrl _ = error "fromUrl applied to non-URL"

-- === Get a list of commands, paired with indentation levels
readSmsnLines :: String -> [(IndentLevel, Expr)]
readSmsnLines s = map f levelsPairedWithAddressedStrings where
  levelsPairedWithAddressedStrings = map countLeadingSpace $ lines s
  f (lev,s) = (lev, smsnLineToExpr $ stripGraphId s)

countLeadingSpace :: String -> (IndentLevel, String)
countLeadingSpace s = (n, s') where
  aSplit = span (== ' ') s
  n = length $ fst $ aSplit
  s' = snd $ aSplit

stripGraphId :: String -> String
  -- assumes input looks like "* :OETCJmx4rJmIR5Pk: bla bla"
  -- keeps only "bla bla"
stripGraphId = drop 21

-- === Substitute URLs into Text values
countUrlSubstitutions :: String -> UrlCount
countUrlSubstitutions s = f 0 s where
  f n (']' : '(' : ')' : s') = f (n+1) s'
  f n (_:s') = f n s'
  f n [] = n

interleave :: [String] -> [String] -> String
interleave outer inner = concat $ tail $ concat -- tail drops the "discard"
  $ map (\(a,b) -> [a,b])
  $ zip ("discard" : inner) outer -- inner is one shorter than outer

substituteUrlsOnce :: String -> [String] -> String
substituteUrlsOnce s urls =  interleave sDivided urlsBracketed
    where sDivided = splitOn "]()" s
          urlsBracketed = map (\url -> "](" ++ url ++ ")") urls

substituteUrls :: [(IndentLevel,Expr)] -> [Expr]
  -- this should consume all URLs
  -- if it does not, the input text has mismatches
substituteUrls levComs = f [] levComs where
  f :: [Expr] -> [(IndentLevel,Expr)] -> [Expr]
  f accumulator ((lev, Text s) : rest) = let
    urlCount = countUrlSubstitutions s
    urlPairs = take urlCount rest
    goodLevels :: String
    goodLevels = if (length $ filter ((== lev+1) . fst) urlPairs)
                    == length urlPairs
      then "it's cool"
      else error "The " ++ show urlCount ++ " URLs following \"" ++ s
           ++ "\" do not all lie one indentation level below it."
    sWithSubs = substituteUrlsOnce s
               $ map (fromUrl . snd)
               $ filter ((== lev+1) . fst) -- ensure URLs are nested under it
                -- PITFALL: if this goes wrong, the error will be cryptic
               $ urlPairs
    in f (Text sWithSubs : accumulator) $ drop urlCount rest
  f accumulator ((lev, com) : rest) = f (com : accumulator) rest
  f accumulator [] = accumulator

substituteUrls_test :: [Expr]
substituteUrls_test = substituteUrls
  [ (2, Text "[frog]() and [goat]() are friends")
  ,  (3, Url "http://frog")
  ,  (3, Url "http://goat")
  ]

substituteUrls_test' :: [Expr]
substituteUrls_test' = substituteUrls
  [ (2, Text "[frog]() and [goat]() are friends")
  ,  (3, Url "http://frog")
  ,  (1, Url "http://goat")
  ]

-- === Handle files
pairFilesToContents :: [Expr] -> [(FilePath, String)]
pairFilesToContents stuff = zip files contents where
  files = map (\(File s) -> s) $ filter isFile stuff
  contents = map buildMarkdownFile $ tail $ splitWhen isFile stuff
    -- use tail to ignore anything before the first filename

main = do
  (inputFile:_) <- getArgs
  input <- readFile inputFile
  let pairs = pairFilesToContents $ substituteUrls $ readSmsnLines input
  writeFile "all makrdown, concatenated.txt" $ unlines $ map snd pairs
  mapM_ f pairs where
    f :: (FilePath, String) -> IO ()
    f (name, content) = writeFile name content

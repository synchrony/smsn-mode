-- run it from the command line:
  -- runghc smsn-to-git-markdown.hs INPUT_FILE

-- how it works
  -- In the text exported from smsn, each block of lines representing a file's contents should start with a note that reads "[target-filename]...". The portion after the ] indicates where that content will be written to. Anything before the first such note is ignored.
  -- Notes with makrdown text content should begin with "[markdown]...", and notes with markdown code, "[markdown-code]".
  -- A note with markdown text content can include incomplete URLs like "see [something cool]() for more information". Such a note should have children, one for each incomplete URL. They will be filled in accordingly. If it has more children yet, the URLs must be first. Each should start with "[url-leaf]...".

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
  f (Url u) = Just $ error $ "Un-substituted URL: <<<" ++ u ++ ">>>"
  f Ignore = Nothing
  -- In markdown, all ordinary text lines need to be preceded by a newline,
  -- but lines of code, including the bracketing ``` lines, do not.

isFile :: Expr -> Bool
isFile (File _) = True
isFile _ = False

isUrl :: Expr -> Bool
isUrl (Url _) = True
isUrl _ = False

fromUrl :: Expr -> String
fromUrl (Url s) = s
fromUrl _ = error "fromUrl applied to non-URL"


-- === Get a list of Exprs, paired with indentation levels
readSmsnLines :: String -> [(IndentLevel, Expr)]
readSmsnLines s = map f levelsPairedWithAddressedStrings where
  levelsPairedWithAddressedStrings = map countLeadingSpace $ lines s
  f (lev,s) = (lev, smsnLineToExpr $ stripGraphId s)

countLeadingSpace :: String -> (IndentLevel, String)
countLeadingSpace s = (n, s') where
  aSplit = span (== ' ') s
  n = round $ (fromIntegral $ length $ fst $ aSplit) / 4
  s' = snd $ aSplit

stripGraphId :: String -> String
  -- assumes input looks like "* :OETCJmx4rJmIR5Pk: bla bla"
  -- keeps only "bla bla"
stripGraphId = drop 21


-- === Substitute URLs into Text values
data SubstitutionGroup = DoNothing Expr
  | Substitute [(IndentLevel, Expr)] -- head should be Text, the rest Urls
  deriving Show

groupForSubs :: [(IndentLevel, Expr)] -> [SubstitutionGroup]
groupForSubs pairs = f [] pairs where
  f :: [SubstitutionGroup] -> [(IndentLevel, Expr)] -> [SubstitutionGroup]
  f acc [] = reverse acc
  f acc ( firstPair@(lev, Text s) : pairs )
    = case countUrlSubstitutions s of
           0 -> f (DoNothing (Text s) : acc) pairs
           n -> f ( (Substitute $ firstPair : take n pairs) : acc )
             $ drop n pairs
  f acc ( (lev,expr) : pairs ) = f (DoNothing expr : acc) pairs

groupForSubs_test = groupForSubs
  [(1, File "something.md")
  ,(1, Text "[subme]() [twice]() yeah")
  ,(2, Url "goat")
  ,(2, Url "frog")
  ,(1, Code "n+3=2")]

substituteUrls :: SubstitutionGroup -> Expr
substituteUrls (DoNothing expr) = expr
substituteUrls (Substitute ((lev, Text s) : urlPairs)) =
  if (all (== lev+1) $ map fst urlPairs) && (all isUrl $ map snd urlPairs)
  then Text $ substituteUrlsOnce s $ map (fromUrl . snd) urlPairs
  else error $ "Bad URL substitution for Text that reads <<<" ++ s ++ ">>>"

substituteUrlsOnce :: String -> [String] -> String
substituteUrlsOnce s urls =  interleave sDivided urlsBracketed
    where sDivided = splitOn "]()" s
          urlsBracketed = map (\url -> "](" ++ url ++ ")") urls

countUrlSubstitutions :: String -> UrlCount
countUrlSubstitutions s = f 0 s where
  f n (']' : '(' : ')' : s') = f (n+1) s'
  f n (_:s') = f n s'
  f n [] = n

interleave :: [String] -> [String] -> String
interleave outer inner = concat $ tail $ concat -- tail drops the "discard"
  $ map (\(a,b) -> [a,b])
  $ zip ("discard" : inner) outer -- inner is one shorter than outer


-- === Handle files
pairFilesToContents :: [Expr] -> [(FilePath, String)]
pairFilesToContents stuff = zip files contents where
  files = map (\(File s) -> s) $ filter isFile stuff
  contents = map buildMarkdownFile $ tail $ splitWhen isFile stuff
    -- use tail to ignore anything before the first filename

main = do
  (inputFile:_) <- getArgs
  input <- readFile inputFile
  let fileContentPairs = pairFilesToContents 
              $ map substituteUrls $ groupForSubs $ readSmsnLines input
  writeFile "all makrdown, concatenated.txt"
    $ unlines $ map snd fileContentPairs
  mapM_ f fileContentPairs where
    f :: (FilePath, String) -> IO ()
    f (name, content) = writeFile name content

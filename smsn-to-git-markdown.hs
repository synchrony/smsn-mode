import Data.List (span)
import System.Process

stripLeadingSpace :: String -> String
stripLeadingSpace = snd . span (== ' ')

-- assumes input looks like "* :OETCJmx4rJmIR5Pk: bla bla", keeps only blas
stripAddress :: String -> String 
stripAddress = drop 20


#!/bin/bash
#
# Set up a directory for generating SmSn/SmSn-mode wiki documentation.
#
# After running this Bash script, you can run the Haskell scripts, e.g.
#   runghc smsn-mode/smsn-to-git-markdown.hs ~/smsn+/input.auto.md 
#
# You will need to have insalled Cabal and the Glasgow Haskell Compiler
# (GHC) first. On Mac OS X, this is as simple as:
#   brew install ghc cabal-install

mkdir smsn-md
cd smsn-md/

git clone git@github.com:synchrony/smsn-mode.git
cd smsn-mode/
git checkout develop
cd ..

git clone git@github.com:synchrony/smsn.wiki.git
git clone git@github.com:synchrony/smsn-mode.wiki.git
git clone git@github.com:synchrony/smsn-why.git


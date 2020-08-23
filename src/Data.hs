module Data where

import Data.List (stripPrefix)

-- | The input of the matchers
type Input = [Char]

-- | The name of a match
type Name = [Char]

-- | The matched part of a match
type Matched = [Char]

-- | The rest part of a match
type Rest = [Char]

-- | The match data
data Match = Match Name Matched [Match] deriving (Show, Eq)

-- | The matcher, a function which can produce a match with a given input
type Matcher = Input -> Maybe Match

-- | The empty match, useful to avoid repetitions
emptyMatch :: Match
emptyMatch = Match "" "" []

-- | Extracts the rest of the given input after the match applies
rest :: Match -> Input -> Rest
rest (Match n m l) i = case stripPrefix m i of
  Just rest -> rest
  _ -> undefined
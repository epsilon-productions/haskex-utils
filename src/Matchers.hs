module Matchers where

import Data

char :: Char -> Matcher
char c = go
  where
    go i = case i of
      (c : r) -> Just $ Match [c] [c] r []
      otherwise -> Nothing
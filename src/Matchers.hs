module Matchers (char, string, oneOrMore, zeroOrMore, optionally, as) where

import Data
import Data.List (stripPrefix)
import Prelude hiding (until)

-- |
-- A matcher which matches the given char
--
-- Examples:
--
-- >>> char 'h' $ "hello"
-- Just (Match "h" "h" "ello" [])
char :: Char -> Matcher
char c = \i -> case i of
  (c : r) -> Just $ Match [c] [c] r []
  otherwise -> Nothing

-- |
-- A matcher which matches the given string
--
-- Examples:
--
-- >>> string "hello" $ "hello world"
-- Just (Match "hello" "hello" " world" [])
string :: [Char] -> Matcher
string s = \i -> case stripPrefix s i of
  Just r -> Just $ Match s s r []
  otherwise -> Nothing

-- |
-- A matcher which matches one or more with the given matcher.
-- Like + quantifier.
--
-- Examples:
--
-- >>> (oneOrMore $ string "ab") "abab"
-- Just (Match "abab" "abab" "" [Match "ab" "ab" "ab" [],Match "ab" "ab" "" []])
oneOrMore :: Matcher -> Matcher
oneOrMore f = \i -> case until f i of
  [] -> Nothing
  matches -> Just $ merge matches

-- |
-- A matcher which matches zero or more with the given matcher.
-- Like * quantifier.
--
-- Examples:
--
-- >>> (zeroOrMore $ string "ab") "abab"
-- Just (Match "abab" "abab" "" [Match "ab" "ab" "ab" [],Match "ab" "ab" "" []])
-- >>> (zeroOrMore $ string "ab") "cdcd"
-- Just (Match "" "" "cdcd" [])
zeroOrMore :: Matcher -> Matcher
zeroOrMore f = optionally $ oneOrMore f

-- |
-- A matcher which matches with the given matcher or matches nothing
-- Like ? quantifier.
--
-- Examples:
--
-- >>> (optionally $ char 'h') "hello world"
-- Just (Match "h" "h" "ello world" [])
-- >>> (optionally $ char 'e') "hello world"
-- Just (Match "h" "h" "ello world" [])
optionally :: Matcher -> Matcher
optionally f = \i -> case f i of
  Just match -> Just match
  otherwise -> Just $ Match "" "" i []

-- |
-- A matcher with the produced match (if any) renamed with the given name
--
-- Examples:
--
-- >>> char 'h' `as` "my char" $ "hello world"
-- Just (Match "my char" "h" "ello world" [])
as :: Matcher -> Name -> Matcher
as f name = \i -> case f i of
  Just (Match n m r l) -> Just $ Match name m r l
  otherwise -> Nothing

-- Utils
----------------------------------------------------------------------------------------------------

until :: Matcher -> Input -> [Match]
until f i = go [] i
  where
    go matches rest = case f rest of
      Just (Match n m r l) -> go (matches ++ [Match n m r l]) r
      Nothing -> matches

merge :: [Match] -> Match
merge matches = go matches ([], [])
  where
    go [] (m, r) = Match m m r matches
    go (Match n matched rest l : ms) (m, r) =
      go ms (m ++ matched, rest)

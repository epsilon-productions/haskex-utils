module Matchers
  ( any,
    char,
    string,
    oneOrMore,
    zeroOrMore,
    optionally,
    not,
    allOf,
    anyOf,
    as,
  )
where

import Controls
import Data
import Data.List (stripPrefix)
import Prelude hiding (any, not, until)

-- |
-- A matcher which matches the next char, whatever it is
--
-- Examples:
--
-- >>> any "hello"
-- Just (Match "h" "h" [])
--
-- >>> any ""
-- Nothing
any :: Matcher
any [] = Nothing
any (c : r) = Just $ Match [c] [c] []

-- |
-- A matcher which matches the given char
--
-- Examples:
--
-- >>> char 'h' $ "hello"
-- Just (Match "h" "h" [])
char :: Char -> Matcher
char c = string [c]

-- |
-- A matcher which matches the given string
--
-- Examples:
--
-- >>> string "hello" $ "hello world"
-- Just (Match "hello" "hello" [])
string :: [Char] -> Matcher
string s i = case stripPrefix s i of
  Just _ -> Just $ Match s s []
  _ -> Nothing

-- |
-- A matcher which matches one or more with the given matcher.
--
-- Like + quantifier.
--
-- Examples:
--
-- >>> (oneOrMore $ string "ab") "abab"
-- Just (Match "abab" "abab" [Match "ab" "ab" [],Match "ab" "ab" []])
oneOrMore :: Matcher -> Matcher
oneOrMore f i = case until f i of
  [] -> Nothing
  matches -> Just $ merge matches

-- |
-- A matcher which matches zero or more with the given matcher.
--
-- Like * quantifier.
--
-- Examples:
--
-- >>> (zeroOrMore $ string "ab") "abab"
-- Just (Match "abab" "abab" [Match "ab" "ab" [],Match "ab" "ab" []])
-- >>> (zeroOrMore $ string "ab") "cdcd"
-- Just (Match "" "cdcd" [])
zeroOrMore :: Matcher -> Matcher
zeroOrMore f = optionally $ oneOrMore f

-- |
-- A matcher which matches with the given matcher or matches nothing
--
-- Like ? quantifier.
--
-- Examples:
--
-- >>> (optionally $ char 'h') "hello world"
-- Just (Match "h" "h" [])
-- >>> (optionally $ char 'e') "hello world"
-- Just (Match "h" "h" [])
optionally :: Matcher -> Matcher
optionally f i = case f i of
  Just match -> Just match
  _ -> Just emptyMatch

-- |
-- A matcher which invert the result of the given matcher.
--
-- - If the given matcher matches, it doesn't.
-- - If the given matcher doesn't match, it does (next char)
--
-- Examples:
--
-- >>> (not $ char 'h') "hello world"
-- Nothing
-- >>> (not $ char 'H') "hello world"
-- Just (Match "" "" [])
not :: Matcher -> Matcher
not f i = case f i of
  Just match -> Nothing
  _ -> any i

-- |
-- A matcher which match if only all of the given matches match.
--
-- Examples:
--
-- >>> allOf [char 'h', char 'e', char 'l', char 'l', char 'o'] "hello world"
-- Just (Match "hello" "hello" [Match "h" "h" [],Match "e" "e" [],Match "l" "l" [],Match "l" "l" [],Match "o" "o" []])
-- >>> allOf [] "hello world"
-- Just (Match "" "" [])
allOf :: [Matcher] -> Matcher
allOf fs = \i -> case go fs [] i of
  Just matches -> Just $ merge matches
  _ -> Nothing
  where
    go [] matches r = Just matches
    go (f : fs) matches r = case f r of
      Just match -> go fs (matches ++ [match]) $ rest match r
      _ -> Nothing

-- |
-- A matcher which match if only all of the given matches match.
--
-- Examples:
--
-- >>> anyOf [char 'a', char 'h', char 'b', char 'c'] "hello world"
-- Just (Match "h" "h" [])
-- >>> anyOf [] "hello world"
-- Just (Match "" "hello world" [])
anyOf :: [Matcher] -> Matcher
anyOf fs = go fs
  where
    go [] i = Nothing
    go (f : fs) i = case f i of
      Just match -> Just match
      _ -> go fs i

-- |
-- A matcher with the produced match (if any) renamed with the given name
--
-- Examples:
--
-- >>> char 'h' `as` "my char" $ "hello world"
-- Just (Match "my char" "h" [])
as :: Matcher -> Name -> Matcher
as f name i = case f i of
  Just (Match n m l) -> Just $ Match name m l
  _ -> Nothing

-- Utils
----------------------------------------------------------------------------------------------------

-- | Apply the same matcher over on over on the input until no match is found, collecting
-- all the matches
until :: Matcher -> Input -> [Match]
until f = go []
  where
    go matches r = case f r of
      Just match -> go (matches ++ [match]) $ rest match r
      Nothing -> matches

-- | Merge the given matches in a parent match
merge :: [Match] -> Match
merge matches = go matches []
  where
    go [] matched = Match matched matched matches
    go (Match n m l : ms) matched =
      go ms (matched ++ m)

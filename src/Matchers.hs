module Matchers
  ( matchIf,
    any,
    char,
    string,
    oneOrMore,
    zeroOrMore,
    optionally,
    not,
    exactly,
    min,
    max,
    between,
    allOf,
    anyOf,
    as,
    digit,
    letter,
    word,
    whitespace,
    tab,
    space,
    newline,
    end,
  )
where

import Controls
import Data
import Data.Char (isDigit, isLetter, isSpace)
import Data.List (stripPrefix)
import Numeric.Natural (Natural)
import Prelude hiding (any, max, min, not, until)

-- Generics
----------------------------------------------------------------------------------------------------

-- |
-- A matcher which matches the next char if it passes the given test function.
--
-- Examples:
--
-- >>> matchIf (\c -> c == 'h') "hello"
-- Just (Match "h" "h" [])
--
-- >>> matchIf (const True) ""
-- Nothing
matchIf :: (Char -> Bool) -> Matcher
matchIf _ [] = Nothing
matchIf test (c : _) = if test c then Just $ Match [c] [c] [] else Nothing

-- |
-- A matcher which matches the next char, whatever it is.
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
-- A matcher which matches the given char.
--
-- Examples:
--
-- >>> char 'h' $ "hello"
-- Just (Match "h" "h" [])
char :: Char -> Matcher
char c = string [c]

-- |
-- A matcher which matches the given string.
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
--
-- >>> (zeroOrMore $ string "ab") "cdcd"
-- Just (Match "" "cdcd" [])
zeroOrMore :: Matcher -> Matcher
zeroOrMore f = optionally $ oneOrMore f

-- |
-- A matcher which matches with the given matcher or matches nothing.
--
-- Like ? quantifier.
--
-- Examples:
--
-- >>> (optionally $ char 'h') "hello world"
-- Just (Match "h" "h" [])
--
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
-- - If the given matcher doesn't match, it does (next char).
--
-- Examples:
--
-- >>> (not $ char 'h') "hello world"
-- Nothing
--
-- >>> (not $ char 'H') "hello world"
-- Just (Match "" "" [])
not :: Matcher -> Matcher
not f i = case f i of
  Just match -> Nothing
  _ -> any i

-- |
-- A matcher which matches exactly n times with the given matcher.
--
-- Examples:
--
-- >>> exactly 3 any $ "hello"
-- Just (Match "hel" "hel" [Match "h" "h" [],Match "e" "e" [],Match "l" "l" []])
--
-- >>> exactly 0 any $ "hello"
-- Just (Match "" "" [])
exactly :: Natural -> Matcher -> Matcher
exactly n f = allOf $ replicate (fromEnum n) f

-- |
-- A matcher which matches at least n times with the given matcher.
--
-- Examples:
--
-- >>> min 3 any $ "hello"
-- Just (Match "hel" "hel" [Match "h" "h" [],Match "e" "e" [],Match "l" "l" []])
--
-- >>> min 0 any $ "hello"
-- Just (Match "" "" [])
--
-- >>> min 6 any $ "hello"
-- Nothing
min :: Natural -> Matcher -> Matcher
min n f i =
  let matches = until f i in if length matches < fromEnum n then Nothing else Just $ merge matches

-- |
-- A matcher which matches at most n times with the given matcher.
--
-- Examples:
--
-- >>> max 5 any $ "hello"
-- Just (Match "hello" "hello" [Match "h" "h" [],Match "e" "e" [],Match "l" "l" [],Match "l" "l" [],Match "o" "o" []])
--
-- >>> max 0 any $ "hello"
-- Nothing
max :: Natural -> Matcher -> Matcher
max n f i =
  let matches = until f i in if length matches <= fromEnum n then Just $ merge matches else Nothing

-- |
-- A matcher which matches with the given matcher a number of times between the given naturals.
--
-- Examples:
--
-- >>> between 1 5 any $ "hello"
-- Just (Match "hello" "hello" [Match "h" "h" [],Match "e" "e" [],Match "l" "l" [],Match "l" "l" [],Match "o" "o" []])
--
-- >>> between 1 5 any $ ""
-- Nothing
--
-- >>> between 5 1 any $ "hello"
-- Nothing
between :: Natural -> Natural -> Matcher -> Matcher
between n1 n2 f i = (\_ -> max n2 f i) =<< min n1 f i

-- |
-- A matcher which matches only if all of the given matches match, in sequence.
--
-- Examples:
--
-- >>> allOf [char 'h', char 'e', char 'l', char 'l', char 'o'] "hello world"
-- Just (Match "hello" "hello" [Match "h" "h" [],Match "e" "e" [],Match "l" "l" [],Match "l" "l" [],Match "o" "o" []])
--
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
-- A matcher which matches only if any of the given matches match.
--
-- Matchers order is important.
--
-- Examples:
--
-- >>> anyOf [char 'a', char 'h', char 'b', char 'c'] "hello world"
-- Just (Match "h" "h" [])
--
-- >>> anyOf [] "hello world"
-- Nothing
anyOf :: [Matcher] -> Matcher
anyOf fs = go fs
  where
    go [] i = Nothing
    go (f : fs) i = case f i of
      Just match -> Just match
      _ -> go fs i

-- |
-- A matcher with the produced match (if any) renamed with the given name.
--
-- Examples:
--
-- >>> char 'h' `as` "my char" $ "hello world"
-- Just (Match "my char" "h" [])
as :: Matcher -> Name -> Matcher
as f name i = case f i of
  Just (Match n m l) -> Just $ Match name m l
  _ -> Nothing

-- Specific
----------------------------------------------------------------------------------------------------

-- |
-- A matcher which matches a digit.
--
-- Examples:
--
-- >>> digit "0"
-- Just (Match "0" "0" [])
digit :: Matcher
digit = matchIf isDigit

-- |
-- A matcher which matches a letter.
--
-- Examples:
--
-- >>> letter "a"
-- Just (Match "a" "a" [])
--
-- >>> letter "A"
-- Just (Match "A" "A" [])
letter :: Matcher
letter = matchIf isLetter

-- |
-- A matcher which matches a word (one or more letters).
--
-- Examples:
--
-- >>> word "hello world"
-- Just (Match "hello" "hello" [Match "h" "h" [],Match "e" "e" [],Match "l" "l" [],Match "l" "l" [],Match "o" "o" []])
--
-- >>> word "a"
-- Just (Match "a" "a" [Match "a" "a" []])
word :: Matcher
word = oneOrMore letter

-- |
-- A matcher which matches any whitespace char.
--
-- Examples:
--
-- >>> whitespace " world"
-- Just (Match " " " " [])
--
-- >>> whitespace "hello"
-- Nothing
whitespace :: Matcher
whitespace = matchIf isSpace

-- |
-- A matcher which matches a tab char.
--
-- Examples:
--
-- >>> tab "\tworld"
-- Just (Match "\t" "\t" [])
tab :: Matcher
tab = char '\t'

-- |
-- A matcher which matches a space char.
--
-- Examples:
--
-- >>> space " world"
-- Just (Match " " " " [])
space :: Matcher
space = char ' '

-- |
-- A matcher which matches a newline.
--
-- All kind of newlines are checked:
--
-- - \\n
-- - \\r
-- - \\n\\r
-- - \\r\\n
--
-- Examples:
--
-- >>> newline "\n"
-- Just (Match "\n" "\n" [Match "\n" "\n" [],Match "" "" []])
--
-- >>> newline "\r"
-- Just (Match "\r" "\r" [Match "\r" "\r" [],Match "" "" []])
--
-- >>> newline "\n\r"
-- Just (Match "\n\r" "\n\r" [Match "\n" "\n" [],Match "\r" "\r" []])
--
-- >>> newline "\r\n"
-- Just (Match "\r\n" "\r\n" [Match "\r" "\r" [],Match "\n" "\n" []])
newline :: Matcher
newline =
  anyOf
    [ allOf [char '\r', char '\n'],
      allOf [char '\n', char '\r'],
      char '\n',
      char '\r'
    ]

-- |
-- A matcher which matches the end of the input, aka when the input has no more chars in it.
--
-- Examples:
--
-- >>> end ""
-- Just (Match "" "" [])
--
-- >>> end "still something"
-- Nothing
end :: Matcher
end [] = Just emptyMatch
end (c : cs) = Nothing

-- Utils
----------------------------------------------------------------------------------------------------

-- | Apply the same matcher over on over on the input until no match is found, collecting.
-- all the matches
until :: Matcher -> Input -> [Match]
until f = go []
  where
    go matches r = case f r of
      Just match -> go (matches ++ [match]) $ rest match r
      Nothing -> matches

-- | Merge the given matches in a parent match.
merge :: [Match] -> Match
merge matches = go matches []
  where
    go [] matched = Match matched matched matches
    go (Match n m l : ms) matched =
      go ms (matched ++ m)

module Data where

type Input = [Char]

type Name = [Char]

type Matched = [Char]

type Rest = [Char]

data Match = Match Name Matched Rest [Match]

type Matcher = Input -> Maybe Match
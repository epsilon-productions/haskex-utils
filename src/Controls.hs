module Controls where

import Data

-- |
-- Apply the matcher on the input, useful as an infix operator
on :: Matcher -> Input -> Maybe Match
on f i = f i
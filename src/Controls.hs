module Controls where

import Data

-- |
-- Apply the matcher on the input, useful as an infix operator for clarity
-- as in:
--
-- >>> matcher `on` input
on :: Matcher -> Matcher
on f = f
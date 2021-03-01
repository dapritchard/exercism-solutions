module Isogram (isIsogram) where

-- add `regex-compat` to the dependencies in package.yaml to install the regular
-- expression dependency
import Text.Regex
import Data.Char
import qualified Data.Set as Set

-- Check if the input is an isogram, defined as a word or phrase without a (case
-- insensitive) repeating letter, however spaces and hyphens are allowed to
-- appear multiple times.
isIsogram :: String -> Bool
isIsogram str =
  let str_nopunct = subRegex (mkRegex "[^[:alpha:]]") str ""
      str_lower = map toLower str_nopunct
      letter_set = Set.fromList str_lower
  in length str_nopunct == Set.size letter_set

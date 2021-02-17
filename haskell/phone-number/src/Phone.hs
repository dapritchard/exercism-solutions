module Phone (number) where

-- add `regex-compat` to the dependencies in package.yaml to install the regular
-- expression dependency
import Data.Maybe
import Text.Regex

-- Clean a string representing a North American Numbering Plan (NANP) telephone
-- number into a canonical form (a 10-digit string). The input may be a 10 or 11
-- digit number where the first digit is a "1", and may also contain any of the
-- following punction characters: "()+-.". The ten-digit number (not including a
-- possible leading "1") must be of the form
--     NXXNXXXXXX
-- where N is any digit from 2 through 9 and X is any digit from 0 through 9.
number :: String -> Maybe String
number = (
  (wrap_maybe_inp check_10digit_form)
  . (wrap_maybe_inp strip_11digit_leading1)
  . (wrap_maybe_inp $ wrap_maybe_outp remove_punct)
  . check_valid_chars
  )

-- Check whether any disallowed characters are included in the string
check_valid_chars :: String -> Maybe String
check_valid_chars str =
  let invalid_regex = mkRegex "[^()+-. [:digit:]]"
      match = matchRegex invalid_regex str
  in if (isNothing match) then (Just str) else Nothing

-- Remove any non-digit characters from the string
remove_punct :: String -> String
remove_punct str = subRegex (mkRegex "[^[:digit:]]") str ""

-- Strip a possible leading "1" from the string
strip_11digit_leading1 :: String -> Maybe String
strip_11digit_leading1 str
  | (str_len /= 11)   = Just str
  | (head str == '1') = Just $ tail str
  | otherwise         = Nothing
  where str_len = length str

-- Check that the string satisfies the NANP 10-digit form
check_10digit_form :: String -> Maybe String
check_10digit_form str =
  let valid_regex = mkRegex "^[2-9][[:digit:]]{2}[2-9][[:digit:]]{6}$"
      match = matchRegex valid_regex str
  in if (isNothing match) then Nothing else (Just str)

-- Convert a function to be able to take a Maybe input
wrap_maybe_inp :: (a -> Maybe b) -> (Maybe a) -> (Maybe b)
wrap_maybe_inp f x =
  case x of
    Just y -> f y
    Nothing -> Nothing

-- Convert a function to return a Maybe output
wrap_maybe_outp :: (a -> b) -> a -> (Maybe b)
wrap_maybe_outp f x = Just $ f x

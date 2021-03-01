{-# LANGUAGE OverloadedStrings #-}
module Acronym (abbreviate) where

import           Data.Char
import qualified Data.Text as T
import           Data.Text (Text)

-- Construct an acronym from the input Text. The acronym letters are defined as
-- being the first letter of every word (including words separated by hyphens),
-- and any capitalized letters mid-word (with the exception of words that are
-- entirely capitalized).
abbreviate :: Text -> Text
abbreviate str =
  -- map all word delimiter characters to ' ', split the Text into a list of
  -- words, remove punctuation, find the per-word acronym letters, and then
  -- recombine
  let delims = T.pack "-"
      str_delims = T.map (transform_delims delims) str
      words_orig = T.words str_delims
      words_onlyletters = map extract_letters words_orig
      words_nonempty = filter (not . T.null) words_onlyletters
      words_acro = map find_word_acro words_nonempty
      acro_comb = foldl T.append T.empty words_acro
  in T.toUpper acro_comb
  where extract_letters = T.filter Data.Char.isAlpha
        transform_delims delims c = if T.any (== c) delims then ' ' else c

-- Find a word's acronym letters.
find_word_acro :: Text -> Text
find_word_acro str
  | check_mixed_case str = T.cons (T.head str) (T.filter Data.Char.isUpper (T.tail str))
  | otherwise            = T.take 1 str
  where check_mixed_case str' =
          (T.any Data.Char.isLower str') && (T.any Data.Char.isUpper str')

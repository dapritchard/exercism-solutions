{-# LANGUAGE OverloadedStrings #-}
module Acronym (abbreviate) where

import           Data.Char
import qualified Data.Text as T
import           Data.Text (Text)


-- convert a phrase to its acronym
abbreviate :: Text -> Text
abbreviate xs =
  let wordList = T.split checkWordDelim xs
      acronymList = map calcWordAcronym wordList
  in T.concat acronymList


-- define the set of characters that we consider to be word delimiters
checkWordDelim :: Char -> Bool
checkWordDelim '\'' = False
checkWordDelim w = not (isAlpha w)


-- constructs the acronym for a given string.  The first letter of the string is
-- always included as well as any letters that are the first capital letter
-- after one or more lowercase letters.
calcWordAcronym :: Text -> Text
calcWordAcronym word =
  let firstLetterUpper = firstLetterToUpper word
      startAcro = WordAcronym { wasPrevCharLower = True, acronym = "" }
      endAcro = T.foldl addToWordAcronym startAcro firstLetterUpper
  in acronym endAcro


-- ensure that the first alphabetic character is uppercase
firstLetterToUpper :: Text -> Text
firstLetterToUpper "" = ""
firstLetterToUpper x
  | isAlpha (T.head x) = T.cons (toUpper (T.head x)) (T.tail x)
  | otherwise          = T.cons (T.head x) (firstLetterToUpper (T.tail x))


-- data structure used to record the state of an acronym construction
data WordAcronym = WordAcronym { wasPrevCharLower :: Bool
                               , acronym :: Text
                               } deriving (Eq, Show)


-- conditionally postpends `a` to `acronym acro` if `a` is a capital letter and
-- the previous letter was a lowercase letter
addToWordAcronym :: WordAcronym -> Char -> WordAcronym
addToWordAcronym acro a

  -- case: not an alphabetic character so just keep the current status
  | not $ isAlpha a = acro

  -- case: current character is lowercase so no need to add to acronym
  | isLower a = WordAcronym {
      wasPrevCharLower = True
      , acronym = acronym acro
      }

  -- case: previous character was lowercase and current character is uppercase,
  -- so add letter to acronym
  | wasPrevCharLower acro = WordAcronym {
      wasPrevCharLower = False
      , acronym = T.snoc (acronym acro) a
      }

  -- case: previous character was uppercase and current character is uppercase,
  -- so no need to add letter to acronym
  | otherwise = WordAcronym {
      wasPrevCharLower = False
      , acronym = acronym acro
      }

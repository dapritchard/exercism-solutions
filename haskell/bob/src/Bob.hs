{-# LANGUAGE MultiWayIf #-}

module Bob (responseFor) where

import Data.Char ( isLower, isSpace, isUpper )

responseFor :: String -> String
responseFor xs =
  let noWhites = stripWhitespace xs
      isQuestn = checkQuestion noWhites
      isYelled = checkYelled noWhites
  in  if
    | null noWhites -> "Fine. Be that way!"
    | not isQuestn && not isYelled -> "Whatever."
    | not isQuestn && isYelled -> "Whoa, chill out!"
    | isQuestn && not isYelled -> "Sure."
    | otherwise -> "Calm down, I know what I'm doing!"

stripWhitespace :: String -> String
stripWhitespace = filter (not . isSpace)

checkQuestion :: String -> Bool
checkQuestion [] = False
checkQuestion xs = last xs == '?'

checkYelled :: String -> Bool
checkYelled xs = op (False, xs)
  where
    op (anyUpper, []) = anyUpper
    op (anyUpper, y:ys)
      | isLower y = False
      | isUpper y = op (True, ys)
      | otherwise = op (anyUpper, ys)

module Diamond (diamond) where

import           Data.Char
import qualified Data.Text as T
import           Data.Text (Text)

-- let capital_a_num = fromEnum 'A' in

diamond :: Char -> Maybe [Text]
diamond letter
  | not $ Data.Char.isUpper letter = Nothing
  | otherwise =
      -- let letter_num = fromEnum letter in
      --   let width =
    Just (map (createLine width) letterNumList)
  where
    letterNum = fromEnum letter
    width = 2 * (letterNum - fromEnum 'A') + 1
    -- Create the list of letters in their numeric representations, first
    -- counting up from 'A' to the input letter, and then counting back down. In
    -- the list that counts up we remove the last letter so as to not have a
    -- duplicate middle row. Note that Haskell's range syntax does indeed
    -- provided the desired results for the edge cases of 'A' and 'B'
    letterNumList = [fromEnum 'A' .. letterNum - 1] ++ [letterNum, letterNum - 1 .. fromEnum 'A']

createLine :: Int -> Int -> Text
createLine width letterNum
  | letterNum == fromEnum 'A' = createLineAYes width
  | otherwise                 = createLineANo width letterNum

createLineAYes :: Int -> Text
createLineAYes width =
  T.concat [padding, T.singleton 'A', padding]
  where
    -- The padding width on either side is the total width minus 1 for the
    -- letter itself, and then divided evenly into two parts on either side
    paddingLen = div (width - 1) 2
    padding = T.replicate paddingLen $ T.singleton ' '

createLineANo :: Int -> Int -> Text
createLineANo width letterNum =
  T.concat [paddingOuter, letter, paddingInner, letter, paddingOuter]
  where
    -- the number of letters between the current letter and 'A'
    letterDist = letterNum - fromEnum 'A'
    -- The padding width between the letters is 1 for B (represented by 1), 3
    -- for letter (represented by 2), 5 for D (represented by 3), and so on
    paddingWidthInner = 2 * letterDist - 1
    paddingInner = T.replicate paddingWidthInner $ T.singleton ' '
    -- The padding width on either side is the inner padding width plus 2 for
    -- the letters themselves, and then divided evenly into two parts on either
    -- side
    paddingWidthOuter = div (width - paddingWidthInner - 2) 2
    paddingOuter = T.replicate paddingWidthOuter $ T.singleton ' '
    letter = T.singleton $ toEnum letterNum

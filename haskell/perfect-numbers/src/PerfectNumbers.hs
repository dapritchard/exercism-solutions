module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

-- Take a positive integer as input and return a classification based on the
-- aliquot sum. The aliquot sum is defined as the sum of all of the factors of a
-- number not including the number itself. A number is then categorized
-- according to the following rules:
--
--     Deficient:  the aliquot sum is less than the number
--     Perfect:    the aliquot sum is equal to the number
--     Abundant:   the aliquot sum is greater than the number

classify :: Int -> Maybe Classification
classify n =
  case calc_aliquot_sum n of
    Nothing -> Nothing
    Just aliquot_sum | (aliquot_sum < n)  -> Just Deficient
    Just aliquot_sum | (aliquot_sum == n) -> Just Perfect
    Just _           | otherwise          -> Just Abundant

-- Calculate the aliquot sum of a positive integer
calc_aliquot_sum :: Int -> Maybe Int
calc_aliquot_sum dividend =
  case calc_factors dividend of
    Nothing -> Nothing
    Just _ | (dividend == 1) -> Just 0
    Just a | otherwise -> Just $ foldl (+) 1 a

-- Take a positive integer as input and return a list containing all factors
-- with the exception of 1 and the number itself
calc_factors :: Int -> Maybe [Int]
calc_factors dividend
  | dividend <= 0 = Nothing
  | dividend <= 3 = Just []
  | otherwise =
      let sqrt_x = sqrt $ fromIntegral dividend :: Float
          max_possible = floor sqrt_x :: Int
          possible_factors = [2 .. max_possible]
          smaller_factors = filter (unbox_bool . check_factor dividend) possible_factors
          larger_factors = map (\x -> div dividend x) (reverse smaller_factors)
          -- If `dividend` is a perfect square, then we will repeat the square
          -- root in both `smaller_factors` and `larger_factors`
          larger_factors_norepeat = if (max_possible * max_possible == dividend)
            then tail larger_factors
            else larger_factors
      in Just $ smaller_factors ++ larger_factors_norepeat

-- Check whether `dividend` is evenly divisible by `divisor`
check_factor :: Int -> Int -> Maybe Bool
check_factor dividend divisor
  | divisor == 0 = Nothing
  | otherwise = Just $ mod dividend divisor == 0

-- Return `True` when a value is `Just True`, or `False` otherwise
unbox_bool :: Maybe Bool -> Bool
unbox_bool (Just True) = True
unbox_bool _ = False

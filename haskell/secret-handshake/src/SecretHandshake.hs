-- You and your fellow cohort of those in the "know" when it comes to binary
-- decide to come up with a secret "handshake".
--
-- 1 = wink
-- 10 = double blink
-- 100 = close your eyes
-- 1000 = jump
--
-- 10000 = Reverse the order of the operations in the secret handshake.

module SecretHandshake (handshake) where

import qualified Data.Map as Map

-- store the cipher, where the key refers to the position of the bit that turns
-- "on or off" the corresponding string
cipher :: Map.Map Int String
cipher =
  Map.fromList [ (1, "wink")
               , (2, "double blink")
               , (3, "close your eyes")
               , (4, "jump")
               ]

-- encore a number into the secret handshake
handshake :: Int -> [String]
handshake n
  | n <= 0 || 32 <= n = []
  | 16 <= n           = handshake' $ n - 16
  | otherwise         = reverse $ handshake' n
  where handshake' n' = map cipher_elt $ decimal_to_binary_pos n'

-- convert a decimal number into a representation where each integer in the
-- return value corresponds to the position of a "1" in the binary
-- representation. For example 5 is represented as 101 in binary form so the
-- return value is [3,1].
decimal_to_binary_pos :: Int -> [Int]
decimal_to_binary_pos n =
  thd $ decimal_to_binary_pos' (n, 1, [])
  where thd (_, _, z) = z

-- driver function for `decimal_to_binary_pos`
decimal_to_binary_pos' :: (Int, Int, [Int]) -> (Int, Int, [Int])
decimal_to_binary_pos' (n, pos, pos_list)
  | n <= 0    = (n, pos, pos_list)
  | otherwise =
    let new_pos_list = if (mod n 2) == 1 then pos : pos_list else pos_list
    in decimal_to_binary_pos' (div n 2, pos + 1, new_pos_list)

-- extract the code corresponding to key `i` from the secret handshake cipher
cipher_elt :: Int -> String
cipher_elt i =
  case Map.lookup i cipher of
    Just a -> a
    Nothing -> ""

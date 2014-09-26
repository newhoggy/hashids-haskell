module Hashids where

encode :: [Int] -> String
encode nums = show nums

decode :: String -> [Int]
decode hashid = read hashid


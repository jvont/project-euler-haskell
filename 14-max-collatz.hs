-- from Project Euler: https://projecteuler.net/problem=14

import Data.List (maximumBy)

collatzLen :: Int -> Int
collatzLen n = let
  go 1 acc = acc + 1
  go n acc
    | even n = go (n `div` 2) (acc + 1)
    | otherwise = go (3 * n + 1) (acc + 1)
  in
    go n 0

collatzMax :: [Int] -> Int
collatzMax [] = 0
collatzMax (x:xs) = fst $ foldl maxPair (x, collatzLen x) xs
  where
    maxPair acc y = if snd acc < len then (y, len) else acc
      where
        len = collatzLen y

main = print (collatzMax [1..999999])

import Data.Function
import Data.List

isPalindrome :: Int -> Bool
isPalindrome x = show x == reverse (show x)

combos :: [Int] -> [(Int, Int)]
combos xs = [ (x, y) | (x:rest) <- tails xs , y <- rest ]

palindromes :: [(Int, Int)] -> [(Int, Int)]
palindromes = filter (isPalindrome . uncurry (*))

maxPair :: [(Int, Int)] -> (Int, Int)
maxPair = maximumBy (compare `on` uncurry (*))

main = do
  let all = palindromes $ combos [888..999]
  print all
  print $ map (uncurry (*)) all

  let maxp = maxPair all
  print maxp
  print $ uncurry (*) maxp

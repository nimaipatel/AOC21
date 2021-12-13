getIncreases :: [Integer] -> Int
getIncreases xs = length $ filter (LT ==) ordering
  where
    getOrdering (x1 : x2 : xs) = compare x1 x2 : getOrdering (x2 : xs)
    getOrdering _ = []
    ordering = getOrdering xs

-- more elegant solution
getIncreases' :: [Integer] -> Int
getIncreases' xs = length $ filter (LT ==) $ zipWith compare xs (tail xs)

main = do
  input <- map read . lines <$> readFile "input.txt"
  print (getIncreases' input)

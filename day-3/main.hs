parseInput :: String -> [[Int]]
parseInput xs = (digitToInt <$>) <$> lines xs

-- yeah I'm relying on exception instead of wrapping in Maybe monad ;)
flipBit :: Int -> Int
flipBit = fromEnum . not . toEnum

digitToInt :: Char -> Int
digitToInt x = fromEnum x - fromEnum '0'

getGamma :: [[Int]] -> [Int]
getGamma nums = map f sumOfAll
  where
    initial = replicate (length $ head nums) 0
    sumOfAll = foldl (zipWith (+)) initial nums
    f x
      | x >= length nums `div` 2 = 1
      | otherwise = 0

binToDec :: [Int] -> Int
binToDec xs = foldl f 0 $ zip (reverse xs) [0 ..]
  where
    f n (x, y) = n + x * 2 ^ y

getCommonestAtIndex :: [[Int]] -> Int -> Int
getCommonestAtIndex xs i = fromEnum $ (s !! i) * 2 >= length xs
  where
    s = foldl1 (zipWith (+)) xs

getOxygen :: [[Int]] -> [Int]
getOxygen xs = getOxygenInner xs 0
  where
    getOxygenInner :: [[Int]] -> Int -> [Int]
    getOxygenInner [x] _ = x
    getOxygenInner xs i = getOxygenInner (filter f xs) (i + 1)
      where
        f x = x !! i == getCommonestAtIndex xs i

getLeastAtIndex :: [[Int]] -> Int -> Int
getLeastAtIndex xs i = flipBit $ getCommonestAtIndex xs i

getCO2 :: [[Int]] -> [Int]
getCO2 xs = getCO2Inner xs 0
  where
    getCO2Inner :: [[Int]] -> Int -> [Int]
    getCO2Inner [x] _ = x
    getCO2Inner xs i = getCO2Inner (filter f xs) (i + 1)
      where
        f x = x !! i == getLeastAtIndex xs i

main = do
  input <- parseInput <$> readFile "input.txt"
  let gamma = getGamma input
      epsilon = map flipBit gamma
      oxygen = getOxygen input
      co2 = getCO2 input
  print (binToDec gamma * binToDec epsilon)
  print (binToDec oxygen * binToDec co2)

parseInput :: String -> [(String, Integer)]
parseInput input = map getTuple $ lines input
  where
    getTuple pair = (head (words pair), read $ words pair !! 1)

-- solution for part 1
getPositionOne :: [(String, Integer)] -> Maybe (Integer, Integer)
getPositionOne = foldl f $ Just (0, 0)
  where
    f (Just (hDisp, vDisp)) (dir, mag)
      | dir == "down" = Just (hDisp, vDisp + mag)
      | dir == "up" = Just (hDisp, vDisp - mag)
      | dir == "forward" = Just (hDisp + mag, vDisp)
    f _ _ = Nothing

-- solution for part 2
getPositionTwo :: [(String, Integer)] -> Maybe (Integer, Integer, Integer)
getPositionTwo = foldl f $ Just (0, 0, 0)
  where
    f (Just (hDisp, vDisp, aim)) (dir, mag)
      | dir == "down" = Just (hDisp, vDisp, aim + mag)
      | dir == "up" = Just (hDisp, vDisp, aim - mag)
      | dir == "forward" = Just (hDisp + mag, vDisp + aim * mag, aim)
    f _ _ = Nothing

main = do
  input <- parseInput <$> readFile "input.txt"
  let Just (x, y, aim) = getPositionTwo input
  print (x * y)

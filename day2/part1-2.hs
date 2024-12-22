import Data.List (elemIndex, findIndex)

data Direction =
    FarUp
  | Up
  | Equal
  | Down
  | FarDown
  deriving Eq

valid :: Direction -> Bool
valid Up   = True
valid Down = True
valid _    = False


directions :: [Int] -> [Direction]
directions list = map direction (pairs list)
  where
    direction :: (Int, Int) -> Direction
    direction (x1, x2) | d >  3    = FarUp   --
                       | d >  0    = Up      -- 0 <  d < 4
                       | d == 0    = Equal   -- 
                       | d > -4    = Down    -- 0 < -d < 4
                       | otherwise = FarDown --
                    where d = x2 - x1

solve :: [[Int]] -> Int
solve input = length $ filter safe input

safe :: [Int] -> Bool
safe line = 
     same (directions line) 
  && all valid (directions line)

solve2 :: [[Int]] -> Int
solve2 input = length $ filter dampened input

dampened :: [Int] -> Bool
dampened input =
     safe input 
  -- Might be better than going through each possible removed index
  -- Or it might not, findBad looks very unoptimised
  || safe (removeAt (findBad input + 0) input)
  || safe (removeAt (findBad input + 1) input)
  || safe (removeAt (findBad input + 2) input)

findBad :: [Int] -> Int
findBad line = 
  case findIndex (not . valid) (directions line) of  
    Just index -> index
    Nothing -> case findIndex (uncurry (/=)) (pairs (directions line)) of
      Just index -> index
      Nothing -> -1


removeAt :: Int -> [t] -> [t]
removeAt index list = take index list ++ drop (index + 1) list

checkDampened :: (Ordering, [Int]) -> Int
checkDampened (dir, values) = length $ filter
    (not . uncurry (checkBounds dir))
    (pairs values)



checkBounds LT x1 x2 = (&&) ((x2-x1) > 0) ((x2-x1) < 4)
checkBounds GT x1 x2 = (&&) ((x1-x2) > 0) ((x1-x2) < 4)
checkBounds EQ x1 x2 = False


parse :: String -> [[Int]]
parse input = do
  let reports = map (map read . words) (lines input)
  reports

main :: IO ()
main = do 
  input <- readFile "input.txt"
  print $ (solve . parse) input
  print $ (solve2 . parse) input

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs xs = zip xs (tail xs)

same :: Eq a => [a] -> Bool
same []  = True
same [_] = True
same list = 
     (head list == (!! 1) list) 
  && same (tail list)
  

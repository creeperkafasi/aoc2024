import Data.Char (isNumber)
solve :: [(Int,Int)] -> Int
solve = sum . map (uncurry (*))

parse :: String -> [(Int, Int)]
parse input = case findMul 0 ("","") input of
  Nothing -> []
  Just ((x,y), rest) -> (x,y) : parse rest

findMul :: Int -> (String,String) -> String -> Maybe ((Int,Int), String)
-- mul(
findMul 0 nums ('m':'u':'l':'(':rest) = findMul 1 nums rest

-- digit after mul(
findMul 1 nums (x':rest) | isNumber x' = findMul 2 ([x'], "") rest
-- digit after digit
findMul 2 (x, _) (x':rest) | isNumber x' = findMul 2 (x++[x'],"") rest

-- comma after digit
findMul 2 nums (',':rest) = findMul 3 nums rest

-- digit after comma
findMul 3 (x, _) (y':rest) | isNumber y' = findMul 4 (x, [y']) rest
-- digit after digit
findMul 4 (x, y) (y':rest) | isNumber y' = findMul 4 (x, y++[y']) rest

-- close after digit
findMul 4 (x,y) (')':rest) = Just ((read x, read y), rest)

-- reset on anything else
findMul _ _ (_:rest) = findMul 0 ("","") rest

-- Nothing if empty
findMul _ _ [] = Nothing


main :: IO ()
main = do
  -- print $ parse "mul(1,23)don't()amul(1,2)do()mul(4,5)"
  input <- readFile "input.txt"
  (print . solve. parse) input


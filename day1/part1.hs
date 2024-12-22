import Data.List (sort)
solve :: [(Int, Int)] -> Int
solve input = sum $ map (abs . uncurry (-)) input

parse :: String -> [(Int, Int)]
parse input =
  zip (sort $ map (read . head) ws)
      (sort $ map (\w -> read (w !! 1)) ws )
  where
    ws = map words (lines input)


main :: IO ()
main = do
  input <- readFile "input.txt"
  print ( (solve.parse) input )
  return ()

solve :: ([Int], [Int]) -> Int
solve input = 
  sum $ map
  (\num -> num * length ( filter (num ==) (snd input) ))
  (fst input)

parse :: String -> ([Int], [Int])
parse input =
  ( map (read . head) ws
  , map (\w -> read (w !! 1)) ws )
  where
    ws = map words (lines input)

main :: IO ()
main = do
  input <- readFile "input.txt"
  print ( (solve.parse) input )
  return ()

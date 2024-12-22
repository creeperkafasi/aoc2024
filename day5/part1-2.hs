import Data.List (findIndex, sortBy, elemIndex)
import Data.Maybe (fromMaybe)
import Control.Arrow (ArrowChoice(right))
import Debug.Trace (trace)
import Data.Text (splitOn, pack, unpack)
import Data.Coerce (coerce)

newtype Rule = Rule (Int, Int)
  deriving (Show, Eq)
newtype Update = Update [Int]
  deriving Show

parse :: String -> ([Rule],[Update])
parse input = mapPair 
  (map parseRule . lines, map parseUpdate . lines)
  (head split, split !! 1)
  where
    split = map unpack $ splitOn (pack "\n\n") (pack input)

    parseRule :: String -> Rule
    parseRule = Rule . readTuple . splitOnce (=='|')

    parseUpdate :: String -> Update
    parseUpdate = Update . map read . splitOn (==',')
      where
        splitOn :: (Char -> Bool) -> String -> [String]
        splitOn _ [] = []
        splitOn del string = left : splitOn del right
          where 
            delPos = fromMaybe (length string) (findIndex del string)
            left = take delPos string
            right = drop (delPos+1) string

solve :: ([Rule], [Update]) -> Int
solve (rules, updates) = (sum . map middle . filter (checkValid rules)) updates

solve2 :: ([Rule], [Update]) -> Int
solve2 (rules, updates) = (sum . map (middle . sortUpdate rules) . filter (not . checkValid rules)) updates

sortUpdate :: [Rule] -> Update -> Update
sortUpdate rules (Update pages) = Update $ sortBy 
      (\p1 p2 -> 
        if Rule (p1,p2) `elem` rules then
          LT
        else if Rule (p2,p1) `elem` rules then 
          GT 
        else
          EQ
      ) pages

checkValid :: [Rule] -> Update -> Bool
checkValid rules (Update pages) = all
  (\pair -> Rule pair `elem` rules)
  (pairs pages)

middle :: Update -> Int
middle (Update pages) = pages !! (length pages `div` 2)

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ (solve . parse) input
  print $ (solve2 . parse) input


mapPair :: (a -> c, b -> d) -> (a,b) -> (c,d)
mapPair (f,g) (a,b) = (f a, g b)

splitOnce :: (a->Bool) -> [a] -> ([a], [a])
splitOnce f list = (left, right)
  where 
    delPos = fromMaybe 0 $ findIndex f list
    left = take delPos list
    right = drop (delPos + 1) list

readTuple :: (String, String) -> (Int, Int)
readTuple (a1, a2) = (read a1, read a2)

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

import System.Random (newStdGen, randomRs)

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)
  | otherwise = y : merge(x:xs) ys

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = let (left, right) = splitAt ((length xs) `div` 2) xs
               in merge (mergeSort left) (mergeSort right)

main :: IO ()
main = do
  ints <- randomIntList 5 (-10) 10
  print $ ints
  print $ mergeSort ints

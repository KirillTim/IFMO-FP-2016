
zipN :: ([a] -> b) -> [[a]] -> [b]
zipN f ([]:xs) = []
zipN f x = f (map head x) : zipN f (map tail x)

main :: IO ()
main = do
  print $ zipN sum [[1, 2, 3],
                    [4, 5, 6],
                    [7, 8, 9]]
  print $ zipN sum [[],
                    [1,2,3]]
  print $ take 5 $ zipN (take 3) $ repeat [0..]
  print $ take 5 $ zipN (take 3) $ repeat [0..1]

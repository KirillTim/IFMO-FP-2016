module Main where

-- implement without MonadPlus and do-notation
whileM :: Monad m => m Bool -> m a -> m [a]
whileM p f = p >>= \x ->
                     if x then
                       f >>= \a -> fmap (a:) (whileM p f)
                     else return []


main :: IO ()
main = do
  putStrLn $ show (whileM [False] [] :: [[Int]]) -- [[]]
  putStrLn $ show (whileM [True] [] :: [[Int]]) -- won't stop

module Main where
import Coin

main :: IO ()
main = do
  let b1 = createCoin blue 1
  let r10 = createCoin red 10
  print $ compareCoins b1 r10

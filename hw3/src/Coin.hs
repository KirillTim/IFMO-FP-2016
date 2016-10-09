{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
module Coin where
newtype Coin color = Coin { getCoin :: Integer }

instance Show (Coin Blue) where
  show (Coin x) = "Blue Coin " ++ show x
instance Show (Coin Red) where
  show (Coin x) = "Red Coin " ++ show x

class Color a where
  coolnes :: a -> Int

instance Color Blue where
  coolnes _ = 1

instance Color Red where
  coolnes _ = 0

data Blue
data Red

blue :: Blue
blue = undefined :: Blue
red :: Red
red  = undefined :: Red

getColor :: Coin color -> color
getColor _ = undefined

compareCoins :: (Color a, Color b) => Coin a -> Coin b -> Ordering
compareCoins x@(Coin a) y@(Coin b)
  | coolnesEq /= EQ = coolnesEq
  | otherwise  = a `compare` b
  where coolnesEq = coolnes (getColor x) `compare` coolnes (getColor y)

createCoin :: color -> Integer -> Coin color
createCoin _ = Coin

instance Monoid (Coin a) where
  mempty                    = Coin 0
  mappend (Coin a) (Coin b) = Coin (a + b)

instance Num (Coin a) where
    (Coin a) + (Coin b) = Coin (a + b)
    (Coin a) * (Coin b) = Coin (a * b)
    abs (Coin a)        = Coin (abs a)
    signum (Coin a)     = Coin (signum a)
    fromInteger a       = Coin (fromInteger a)
    negate (Coin a)     = Coin (negate a)

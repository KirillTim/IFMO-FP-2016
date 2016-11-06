{-# LANGUAGE DuplicateRecordFields #-}

type Attack  = Int
type Defense = Int
type Health  = Int
type Name    = String

data Equipment = Armor {_defense :: Defense} | Sword {_attack :: Attack}
  deriving Show

data Player = Player
  { name    :: Name,
    health  :: Health,
    attack  :: Attack,
    defense :: Defense
  } deriving Show

data Monster = Monster
    { loot       :: Equipment,
      __attack     :: Int,
      __health     :: Int
    } deriving Show

getLoot :: Player -> Equipment -> Player
getLoot p (Armor x) = p {defense = max x (defense p)}
getLoot p (Sword x) = p {attack = max x (attack p)}

fight :: Player -> Monster -> Player
fight player monstr = if pt > mt then error "You are dead!"
                      else getLoot player {defense = max (defense player - dmg) 0} $ loot monstr
  where
    pt = (__health monstr) `div` (attack player)
    mt = ((health player) + (defense player)) `div` (__attack monstr)
    dmg = pt * (__attack monstr)

gloriousBattle :: Player -> [Monster] -> Player
gloriousBattle player []     = player
gloriousBattle player (x:xs) = gloriousBattle (fight player x) xs


safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit x = Just $ init x

strip :: [a] -> [a]
strip x = case (safeInit x) of
            Nothing -> []
            Just y -> case (safeTail y) of
              Nothing -> []
              Just res -> res

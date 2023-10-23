module Assoc where

type Assoc k v  = [(k, v)]


search :: k -> Assoc k v -> Maybe v --if the key is not in Inputs, it should return nothing.
search k list = foldr f Nothing
    where
        f (k', u) may = case may of
            Nothing -> if k == k' then Just u else Nothing
            _       -> may


updateInputs :: k -> v -> Assoc k v -> Assoc k v
updateInputs k v = foldr f []
    where
        f (x, y) xs | y == v    = (k,y) : xs
                    | otherwise = (x,y) : xs 


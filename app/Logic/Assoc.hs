module Assoc where

type Assoc k v  = [(k, v)]


search ::Eq k => k -> Assoc k v -> Maybe v --if the key is not in the Assoc, it should return nothing.
search k list = foldr f Nothing list
    where
        f (k', u) may = case may of
            Nothing -> if k == k' then Just u else Nothing
            _       -> may


updateInputs :: (Eq k, Eq v) => k -> v -> Assoc k v -> Assoc k v
updateInputs k v = foldr f []
    where
        f (newKey, newValue) xs | newValue == value    = (key, newValue) : xs
                                | otherwise            = (newKey, newValue) : xs 

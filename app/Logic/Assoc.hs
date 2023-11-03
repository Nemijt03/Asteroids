module Assoc where

type Assoc k v  = [(k, v)]

-- search for the value corresponding to the key given
search ::Eq k => k -> Assoc k v -> Maybe v --if the key is not in the Assoc, it should return nothing.
search k = foldl f Nothing 
    where
        f may (k', u) = case may of
            Nothing -> if k == k' then Just u else Nothing
            _       -> may

-- search but only when the answer is guaranteed
unsafeSearch :: Eq k => k -> Assoc k v -> v
unsafeSearch k list = head [v |(k',v)<-list, k==k']

-- update the key corresponding to the value given in the (Assoc k v)
updateKey :: (Eq k, Eq v) => k -> v -> Assoc k v -> Assoc k v
updateKey k v = foldr f []
    where
        f (k', v') xs | v == v'    = (k, v') : xs
                      | otherwise  = (k',v') : xs 

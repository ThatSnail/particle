module Algorithm (
      binarySearchLower
    ) where

-- Divide and conquer search on sorted list
-- If doesn't exist in list, round down to closest
binarySearchLower :: (Ord a) => a -> [a] -> Maybe Int
binarySearchLower x xs
    | x >= head xs = Just $ binarySearchLower' x xs ((length xs) `div` 2)
    | otherwise    = Nothing
    where
        binarySearchLower' x _ 0 = 0
        binarySearchLower' x xs i
            | x < (xs !! i) = binarySearchLower' x (take i xs) (i `div` 2)
            | otherwise     = (binarySearchLower' x (drop i xs) ((l - i) `div` 2)) + i
            where
                l = length xs
                hl = l `div` 2

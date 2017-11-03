module MergeSort where

fstHalf :: [a] -> [a]
fstHalf xs = take (length xs `div` 2) xs

sndHalf :: [a] -> [a]
sndHalf xs = drop (length xs `div` 2) xs

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) 
        | (x <= y) = x:(merge xs (y:ys))
        | otherwise = y: (merge (x:xs) ys)

-- merge sort on string lists
merge2 :: Ord a => [a] -> [a] -> [a]
merge2 [] ys = ys
merge2 xs [] = xs
merge2 (x:xs) (y:ys)
    | x <= y = x : merge2 xs (y:ys)
    | otherwise = y : merge2 (x:xs) ys

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs
    = merge2 (mergeSort top) (mergeSort bottom)
    where
    (top, bottom) = splitAt (length xs `div` 2) xs
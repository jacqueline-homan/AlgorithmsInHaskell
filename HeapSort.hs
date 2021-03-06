module HeapSort where 

import Data.List

swap :: Int -> Int -> [a] -> [a]
swap i j xs | i == j    = xs
            | otherwise = initial ++ (xs !! b) : middle ++ (xs !! a) : end 
    where [a,b] = sort [i,j]
          initial = take a xs
          middle  = take (b-a-1) (drop (a+1) xs)
          end     = drop (b+1) xs

largest :: Ord a => Int -> Int -> [a] -> Int
largest i hs xs = 
        let large = if (l < hs) && ((xs !! l) > (xs !! i)) then l else i
        in if (r < hs) && ((xs !! r) > (xs !! large)) then r else large
        where l = 2 * i + 1
              r = 2 * i + 2

heapify :: Ord a => Int -> Int -> [a] -> [a]
heapify i hs xs = 
        if (large /= i) then heapify large hs (swap large i xs)
        else xs
            where large = largest i hs xs

buildheap :: Ord a => Int -> [a] -> [a]
buildheap 0 xs = heapify 0 (length xs) xs
buildheap i xs = buildheap (i - 1) (heapify i (length xs) xs)

hpsort i xs = let swapped = swap 0 i xs
              in if i /= 1 then hpsort (i - 1) (heapify 0 i swapped)
                 else (heapify 0 i swapped)

heapsort xs = let heap = buildheap (length xs `div` 2) xs
              in hpsort (length xs - 1) heap

main :: IO()
main = do 
    let input = [16,4,10,14,7,9,3,2,8]
    print $ heapsort input
-- heapsort [16,4,10,14,7,9,3,2,8]

main2 :: IO() 
main2 = do 
    let myList = [4,4,4,5,5,4]
    print $ group myList

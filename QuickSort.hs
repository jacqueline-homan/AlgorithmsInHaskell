module QuickSort where
sort :: Ord a => [a] -> [a]
sort [] = []
sort (pivot:rest) =
             (sort lesser)
          ++ [pivot]
          ++ (sort greater)
  where
    lesser  = filter (< pivot) rest
    greater = filter (>= pivot) rest

main = do
  let input = [2,4,3,1,4] :: [Integer]
  let sorted = sort input
  let n = length sorted
  let middle = n `div` 2 
  let median =  
        if even n 
        then (fromIntegral (sum [take n sorted,  drop n sorted])) / 2.0
        else drop (n - 1) sorted
        print $ sort input 

module InsertionSort where 

sort :: Ord a => [a] -> [a]
sort xs = inserts xs []

-- Insert given elements in an emerging result
inserts :: Ord a => [a] -> [a] -> [a]
inserts [] r = r
inserts (x:xs) r = inserts xs (insert x r)

-- Insert a given element in a list
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) =
  if x <= y
    then x : y : ys
    else y : insert x ys

main :: IO()    
main = do
    let input = [2,4,3,1,4]
    print $ sort input -- [1,2,3,4,4]
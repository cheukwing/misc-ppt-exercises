mergeSort :: (Ord a) => [a] -> [a]
mergeSort ls
  = mergeSort' ls (length ls)
  where
    mergeSort' :: (Ord a) => [a] -> Int -> [a]
    mergeSort' [] _
      = []
    mergeSort' [x] _
      = [x]
    mergeSort' xs len
      = merge (mergeSort' left leftLength) (mergeSort' right rightLength)
      where
        (left, right) = splitAt leftLength xs
        splitPos      = fromIntegral(len) / 2
        leftLength    = floor splitPos
        rightLength   = ceiling splitPos

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs []
  = xs
merge [] ys
  = ys
merge left @ (x : xs) right @ (y : ys)
  | x > y     = y : merge left ys
  | otherwise = x : merge xs right

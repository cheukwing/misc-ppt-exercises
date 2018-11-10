coins :: [Int]
coins
  = [200, 100, 50, 20, 10, 5, 2, 1]

coinChange :: Int -> [Int] -> Int
coinChange 0 _
  = 0
coinChange amount (c : cs) 
  | amount >= c = div amount c + coinChange (mod amount c) cs
  | otherwise   = coinChange amount cs

coinChange2 :: Int -> [Int] -> Int
coinChange2 amount coins
  = coinChange' amount (filter (<= amount) coins) 0
  where
    coinChange' :: Int -> [Int] -> Int -> Int
    coinChange' 0 _ n
      = n
    coinChange' a (c : cs) n
      = coinChange' a' (filter (<= a') cs) (n + div a c)
      where
        a' = mod a c

coinChange3 :: Int -> [Int] -> Int
coinChange3 amount coins
  = snd (foldl (\(a, ch) c -> (mod a c, ch + div a c)) (amount, 0) coins)

coinChangeDP :: Int -> [Int] -> Int
coinChangeDP 0 _
  = 0
coinChangeDP amount coins
  = coinChangeDP' 1 initTable
  where
    initTable =  take (amount + 1) (repeat 0)
    coinChangeDP' :: Int -> [Int] -> Int
    coinChangeDP' n table
      | n >= amount = change
      | otherwise   = coinChangeDP' (n + 1) newTable
      where
        change = minimum(map (\c -> 1 + table !! (n - c)) (filter (<= n) coins))
        newTable = (take n table) ++ [change] ++ (drop (n + 1) table)


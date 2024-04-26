suffix :: Int -> [a] -> [a]
suffix _ [] = []
suffix n (x:xs) = if (n==0) then x:xs else suffix (n-1) xs

suffixes0 :: Int -> Int -> [a] -> [[a]]
suffixes0 i n xs = if (i<n) then [suffix i xs]++(suffixes0 (i+1) n xs) else [[]] 

suffixes :: [a] -> [[a]]
suffixes xs = suffixes0 0 (length xs) xs
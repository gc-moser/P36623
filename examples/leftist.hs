data Heap a = Empty | Node Int a (Heap a) (Heap a) deriving (Eq, Show, Ord)

isEmpty Empty = True
isEmpty otherwise = False

rank :: Ord a => (Heap a) -> Int
rank Empty = 0
rank (Node rank _ _ _) = rank

makeT :: Ord a => a -> (Heap a) -> (Heap a) -> (Heap a)
makeT x a b = if rank a >= rank b then (Node (rank b + 1) x a b)
            else (Node (rank a +1) x b a)
      
merge :: Ord a => Heap a -> Heap a -> Heap a
merge h1 h2 = case h1 of
      Empty -> h2
      (Node _ x a1 b1) -> case h2 of
         Empty -> h1
         (Node _ y a2 b2) -> if x <= y then (makeT x a1 (merge b1 h2)) else (makeT y a2 (merge h1 b2))

insert :: Ord a => a -> Heap a -> Heap a
insert x h = merge (Node 1 x Empty Empty) h

findMin :: Ord a => Heap a -> a
findMin (Node _ x a b) = x

deleteMin :: Ord a => Heap a -> Heap a
deleteMin (Node _ _ a b) = merge a b

fromList :: Ord a => [a] -> Heap a
fromList ls = case ls of
      [] -> Empty
      (x:xs) -> let hls = map (\ n -> (Node 0 n Empty Empty)) ls in
                foldl merge Empty hls
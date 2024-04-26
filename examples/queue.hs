data Queue a = BQ [a] [a] deriving Show

empty = BQ [] []
isEmpty (BQ f r) = null f

check [] r = BQ (reverse r) []
check f r = BQ f r

snoc (BQ f r) x = check f (x:r)

head (BQ [] _) = error "empty queue"
head (BQ (x:f) r) = x

tail (BQ [] _) = error "empty queue"
tail (BQ (x:f) r) = check f r

fromList :: [a] -> Queue a
fromList [] = empty
fromList (x:xs) = snoc (fromList xs) x
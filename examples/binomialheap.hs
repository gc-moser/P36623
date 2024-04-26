data Tree a = Node Int a [Tree a] deriving Show
-- data Heap a = [Tree a] deriving Show

rank (Node r x c) = r
root (Node r x c) = x

link t1@(Node r x1 c1) t2@(Node _ x2 c2) = if x1 <= x2 then Node (r+1) x1 (t2:c1) else Node (r+1) x2 (t1:c2)

insTree t [] = [t]
insTree t ts@(t' : ts') = if rank t < rank t' then t:ts else insTree (link t t') ts'

insert x ts = insTree (Node 0 x []) ts

merge ts1 [] = ts1
merge [] ts2 = ts2
merge ts1@(t1:ts'1) ts2@(t2:ts'2)
   | rank t1 < rank t2 = t1 : merge ts'1 ts2
   | rank t2 < rank t1 = t2 : merge ts1 ts'2
   | otherwise = insTree (link t1 t2) (merge ts'1 ts'2)

removeMinTree [] = error "empty heap"
removeMinTree [t] = (t, [])
removeMinTree (t:ts) = if root t < root t' then (t,ts) else (t',t:ts')
   where (t',ts') = removeMinTree ts

findMin ts = root t
   where (t,_) = removeMinTree ts

deleteMin ts = merge (reverse ts1) ts2
   where (Node _ x ts1, ts2) = removeMinTree ts

sizetree (Node _ _ []) = 1
sizetree (Node _ _ ts) = size ts

size [] = 0
size (t:ts) = (sizetree t) + (size ts)

zero = (Node 0 0 [])
one = link zero zero
two = link one one
three = link two two

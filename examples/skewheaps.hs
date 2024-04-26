data Skew = Empty | Node(Int,Skew,Skew) deriving Show

empty = Empty

insert(x,s) = merge(Node(x,Empty,Empty),s)

findMin(Node(x,s1,s2)) = x

deleteMin(Node(x,s1,s2)) = merge(s1,s2)

merge(s1,Empty) = s1
merge(Empty,s2) = s2
merge(Node(x,s1,s2),Node(y,t1,t2)) =
  if x < y then Node(x,merge(Node(y,t1,t2),s2),s1)
           else Node(y,merge(Node(x,s1,s2),t2),t1)

natSum 0 = 0
natSum n = n + natSum (n - 1)

--foo 0 h = []
--foo n h  = insert(7,h) : foo (n - 1) h

foo 0 h  = empty
foo n h  = let s=insert(n,h) in foo (n-1) h

foo' 0 h = []
foo' n h  = let s=insert(n,h) in s : foo' (n-1) h

strict_foo 0 h  = empty
strict_foo n h  = insert(n,h) `seq` strict_foo (n-1) h

strict_foo2 n = let s = insert(6, insert(1, insert(6, insert(2, insert(6, insert(3, insert(6, insert(4, insert(6, insert(5,empty))))))))))
         in strict_foo n s

okasaki = insert(6, insert(1, insert(6, insert(2, insert(6, insert(3, insert(6, insert(4, insert(6, insert(5,empty))))))))))


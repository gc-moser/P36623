import System.Random

-- coin flips to simulate geometric distribution
data Coin = Heads | Tails deriving (Show, Enum, Bounded)

instance Random Coin where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')
  random g = randomR (minBound, maxBound) g

-- actual zip tree implementation, using a global list of sampled values
data Tree a = Leaf | Node Int a (Tree a) (Tree a) deriving (Eq, Show)

type RList = [Int]

treeRank (Node r _ _ _) = r
treeKey (Node _ key _ _) = key

insert :: Ord a => Int -> a -> Tree a -> Tree a
insert newrank xkey Leaf = Node newrank xkey Leaf Leaf
insert newrank xkey (Node rank key left right)
   | xkey < key = case (insert newrank xkey left) of
       x@(Node xrank xkey xleft xright) ->
          if (xrank < rank) then
             Node rank key x right
          else
             Node xrank xkey xleft (Node rank key xright right)           
   | xkey >= key = case (insert newrank xkey right) of
       x@(Node xrank xkey xleft xright) ->
          if (xrank <= rank) then
             Node rank key left x
          else
             Node xrank xkey (Node rank key left xleft) xright 

insertLs :: Ord a => RList -> [a] -> Tree a
insertLs rs ls = case (null rs) of
   True -> Leaf
   False -> case ls of
      [] -> Leaf
      (l:ls) -> insert (head rs) l (insertLs (tail rs) ls)

ziptrees :: Ord a => Tree a -> Tree a -> Tree a
ziptrees x Leaf = x
ziptrees Leaf y = y
ziptrees x@(Node xrank xkey xleft xright) y@(Node yrank ykey yleft yright) =
  if xrank < yrank then
     Node yrank ykey (ziptrees x yleft) yright
  else
     Node xrank xkey xleft (ziptrees xright y)

delete :: Ord a => a -> Tree a -> Tree a
{- delete xkey (Node rank key l r) = case (xkey == key) of
   True -> ziptrees l r
   False -> case (xkey < key) of
      True -> case l of
                (Node lrank lkey lleft lright) -> if (xkey == lkey) then
                     Node rank key (ziptrees lleft lright) r
                   else
                     Node rank key (delete xkey l) r
      False -> case r of
                (Node rrank rkey rleft rright) -> if (xkey == rkey) then
                     Node rank key l (ziptrees rleft rright)
                   else
                     Node rank key l (delete xkey r) -}
delete xkey (Node rank key l r) = case (xkey == key) of
   True -> ziptrees l r
   False -> case (xkey < key) of
      False -> Node rank key l (delete xkey r)
      True -> Node rank key (delete xkey l) r 

-- randomisation preparations
count = 10000
maxRank = 10

height :: [Coin] -> Int
height [] = 0
height (c:cs) = case c of
  Heads -> 0
  Tails -> 1 + (height cs)
  
process :: [Coin] -> RList
process [] = [] 
process cs@(c:_) = let h = height $ take maxRank cs in
  h:(process (drop maxRank cs))
  
main :: IO ()
main = do
   putStrLn "Tree"
   g <- newStdGen
   print $ insertLs (process . take count $ (randoms g :: [Coin])) ['a', 'b', 'c']

-- examples

tree = Node 4 'f'
         (Node 2 'd' Leaf Leaf)
         (Node 4 's'
            (Node 2 'h'
               (Node 0 'g' Leaf Leaf) 
               (Node 2 'm'
                  (Node 1 'l'
                     (Node 0 'j' Leaf Leaf)
                     Leaf
                  )
                  Leaf
               )
            )
            (Node 2 'x' Leaf Leaf)
         )

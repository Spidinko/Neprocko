--18.6.2012
--haskell 2

data Tree a = Node a [Tree a]
	deriving (Show)
data BTree a = Nil | BNode a (BTree a) (BTree a)
	deriving (Show)

convert :: Tree a -> BTree a
convert (Node a []) = BNode a Nil Nil
convert (Node a trees) = let btrees = map convert trees in
	insertAll btrees (BNode a Nil Nil)
	where
	insertAll :: [BTree a] -> BTree a -> BTree a
	insertAll [] tree = tree
	insertAll (x:xs) Nil = insertAll xs x
	insertAll xs (BNode a l r) = let len = (length xs) `div` 2 in
		BNode a (insertAll (take len xs) l) (insertAll (drop len xs) r)
